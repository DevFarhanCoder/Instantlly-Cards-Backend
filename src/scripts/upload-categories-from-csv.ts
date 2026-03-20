/**
 * upload-categories-from-csv.ts
 *
 * Reads the "Category Saad - Tier-1-Cities.csv" (or any CSV with same columns):
 *   (indicator) | Name | locatcity | Main Category | Sub Category Of Just dial | Mobile no | City
 *
 * The file has 3 blank rows at the top, a metadata row ("23143,..."), then data.
 * The header row itself starts with an empty first column.
 *
 * Rules applied before uploading:
 *   ✅ Header row is auto-detected (scanned for a row containing "Main Category").
 *   ✅ All rows BEFORE the header are skipped automatically.
 *   ✅ Rows where Mobile no = "Show Number" are SKIPPED (hidden numbers from JustDial).
 *   ✅ Rows where "Name" is purely numeric (row-number artifacts) are SKIPPED.
 *   ✅ Rows with an empty "Main Category" are SKIPPED.
 *   ✅ Rows where ALL fields are empty/blank are SKIPPED.
 *   ✅ Duplicate (Main Category, Sub Category) pairs are deduplicated automatically.
 *   ✅ Already-existing categories/subcategories in DB are left untouched (upsert-safe).
 *
 * Usage:
 *   npx ts-node src/scripts/upload-categories-from-csv.ts "./path/to/file.csv"
 *   npx ts-node src/scripts/upload-categories-from-csv.ts   (uses default file below)
 */

import dotenv from "dotenv";
import path from "path";
dotenv.config({ path: path.join(__dirname, "../../.env") });

import fs from "fs";
import mongoose from "mongoose";
import Category from "../models/Category";

// ─────────────────────────────────────────────────────────────────────────────
// Default CSV path  (override with CLI arg)
// ─────────────────────────────────────────────────────────────────────────────
const DEFAULT_CSV = path.join(
  __dirname,
  "../../../Instantlly-admin/Category Saad - Tier-1-Cities.csv"
);
const csvPath = process.argv[2] ? path.resolve(process.argv[2]) : DEFAULT_CSV;

// ─────────────────────────────────────────────────────────────────────────────
// Helpers
// ─────────────────────────────────────────────────────────────────────────────
const isNumericOnly = (s: string) => /^\s*\d+\s*$/.test(s);
const isShowNumber  = (s: string) => s.trim().toLowerCase() === "show number";

/** Normalize header for comparison: lowercase + collapse whitespace */
function normaliseHeader(h: string): string {
  return h.toLowerCase().replace(/\s+/g, " ").trim();
}

function findHeader(keys: string[], targets: string[]): string | undefined {
  return keys.find((k) => targets.some((t) => normaliseHeader(k) === t));
}

// ─────────────────────────────────────────────────────────────────────────────
// CSV parser
//   - Splits on commas (respects double-quoted fields containing commas).
//   - AUTO-DETECTS the header row by scanning for any line that contains
//     "Main Category" (case-insensitive). Everything before that line is
//     ignored, so leading blank/metadata rows are handled automatically.
// ─────────────────────────────────────────────────────────────────────────────
function parseCSV(raw: string): Record<string, string>[] {
  const lines = raw.split(/\r?\n/).map((l) => l.trimEnd());

  const splitRow = (line: string): string[] => {
    const cells: string[] = [];
    let cur = "";
    let inQ = false;
    for (const ch of line) {
      if (ch === '"') { inQ = !inQ; }
      else if (ch === "," && !inQ) { cells.push(cur.trim()); cur = ""; }
      else { cur += ch; }
    }
    cells.push(cur.trim());
    return cells;
  };

  // Find the header row: first line whose cells contain "main category"
  let headerIdx = -1;
  let headers: string[] = [];
  for (let i = 0; i < lines.length; i++) {
    const cells = splitRow(lines[i]).map((c) => c.replace(/^"|"$/g, "").trim());
    if (cells.some((c) => normaliseHeader(c) === "main category")) {
      headerIdx = i;
      headers = cells;
      break;
    }
  }

  if (headerIdx === -1) {
    console.error('❌  Could not find a header row containing "Main Category".');
    return [];
  }

  console.log(`   Header row found at line ${headerIdx + 1}`);
  console.log(`   Columns: ${headers.filter(Boolean).join(" | ")}`);

  const rows: Record<string, string>[] = [];
  for (const line of lines.slice(headerIdx + 1)) {
    // Skip completely blank or all-comma lines
    if (!line.trim() || line.replace(/,/g, "").trim() === "") continue;

    const vals = splitRow(line);
    const row: Record<string, string> = {};
    headers.forEach((h, i) => {
      row[h] = (vals[i] ?? "").replace(/^"|"$/g, "").trim();
    });
    rows.push(row);
  }
  return rows;
}

// ─────────────────────────────────────────────────────────────────────────────
// Main
// ─────────────────────────────────────────────────────────────────────────────
async function main() {
  // ── 1. Read CSV ────────────────────────────────────────────────────────────
  if (!fs.existsSync(csvPath)) {
    console.error(`❌  CSV file not found: ${csvPath}`);
    console.error(
      `   Usage: npx ts-node src/scripts/upload-categories-from-csv.ts <path-to-csv>`
    );
    process.exit(1);
  }

  const raw = fs.readFileSync(csvPath, "utf-8");
  const allRows = parseCSV(raw);
  console.log(`\n📄  Total data rows after header: ${allRows.length}`);

  if (allRows.length === 0) {
    console.error("❌  No data rows found in CSV.");
    process.exit(1);
  }

  // ── 2. Detect column names ─────────────────────────────────────────────────
  const keys = Object.keys(allRows[0]);

  const nameCol    = findHeader(keys, ["name"]);
  const mainCatCol = findHeader(keys, ["main category", "maincategory", "main_category"]);
  const subCatCol  = findHeader(keys, [
    "sub category of just dial",
    "sub category",
    "subcategory",
    "sub_category",
    "sub category of justdial",
    "sub category of just-dial",
  ]);
  const mobileCol  = findHeader(keys, ["mobile no", "mobile", "mobile_no", "phone", "mobile number"]);

  if (!mainCatCol) {
    console.error(`❌  Could not find "Main Category" column.\n   Found: ${keys.join(", ")}`);
    process.exit(1);
  }

  console.log(`\n   Main Category column : "${mainCatCol}"`);
  console.log(`   Sub Category column  : "${subCatCol ?? "(none — subcategories skipped)"}"`);
  console.log(`   Name column          : "${nameCol ?? "(none — numeric filter disabled)"}"`);
  console.log(`   Mobile column        : "${mobileCol ?? "(none — Show Number filter disabled)"}"`);
  console.log();

  // ── 3. Filter & collect unique Main Category → Sub-Category pairs ──────────
  let skippedNumeric    = 0;
  let skippedShowNumber = 0;
  let skippedNoMainCat  = 0;

  // Map: mainCategoryName → Set<subcategoryName>
  const categoryMap = new Map<string, Set<string>>();

  for (const row of allRows) {
    // Skip rows where Mobile no = "Show Number"
    if (mobileCol) {
      const mobile = row[mobileCol] ?? "";
      if (isShowNumber(mobile)) {
        skippedShowNumber++;
        continue;
      }
    }

    // Skip rows where "Name" is purely numeric (row-number artifacts)
    if (nameCol) {
      const nameVal = row[nameCol] ?? "";
      if (isNumericOnly(nameVal)) {
        skippedNumeric++;
        continue;
      }
    }

    const mainCat = (row[mainCatCol] ?? "").trim();
    if (!mainCat || isNumericOnly(mainCat)) {
      skippedNoMainCat++;
      continue;
    }

    if (!categoryMap.has(mainCat)) {
      categoryMap.set(mainCat, new Set());
    }

    if (subCatCol) {
      const subCat = (row[subCatCol] ?? "").trim();
      if (subCat && !isNumericOnly(subCat) && !isShowNumber(subCat)) {
        categoryMap.get(mainCat)!.add(subCat);
      }
    }
  }

  console.log(`🔍  Filter summary:`);
  console.log(`    Rows skipped (Show Number / hidden phone) : ${skippedShowNumber}`);
  console.log(`    Rows skipped (numeric Name artifact)      : ${skippedNumeric}`);
  console.log(`    Rows skipped (no Main Category)           : ${skippedNoMainCat}`);
  console.log(`    Unique Main Categories                    : ${categoryMap.size}`);
  let totalSubs = 0;
  categoryMap.forEach((subs) => (totalSubs += subs.size));
  console.log(`    Total unique Sub-Categories               : ${totalSubs}`);
  console.log();

  if (categoryMap.size === 0) {
    console.error("❌  No valid categories found. Nothing to upload.");
    process.exit(1);
  }

  // Print full category/sub-category preview
  console.log("📋  Categories to upload:");
  categoryMap.forEach((subs, cat) => {
    const subList = Array.from(subs).sort();
    console.log(`    • ${cat}  (${subList.length} sub-categor${subList.length !== 1 ? "ies" : "y"})`);
    if (subList.length <= 6) {
      subList.forEach((s) => console.log(`        └─ ${s}`));
    } else {
      subList.slice(0, 5).forEach((s) => console.log(`        └─ ${s}`));
      console.log(`        └─ … and ${subList.length - 5} more`);
    }
  });
  console.log();

  // ── 4. Connect to MongoDB ──────────────────────────────────────────────────
  const mongoUri = process.env.MONGODB_URI || process.env.MONGO_URI;
  if (!mongoUri) {
    console.error("❌  MONGODB_URI is not set in .env");
    process.exit(1);
  }

  console.log("🔌  Connecting to MongoDB…");
  await mongoose.connect(mongoUri);
  console.log("✅  Connected\n");

  // ── 5. Upload ──────────────────────────────────────────────────────────────
  const stats = { created: 0, alreadyExisted: 0, subsAdded: 0, subsSkipped: 0 };

  for (const [mainCatName, subsSet] of categoryMap) {
    const subcategories = Array.from(subsSet).sort();

    let category = await Category.findOne({
      name: {
        $regex: new RegExp(
          `^${mainCatName.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}$`,
          "i"
        ),
      },
    });

    if (!category) {
      // Determine next order value among root categories
      const maxOrderDoc = await Category.findOne({ parent_id: null })
        .sort({ order: -1 })
        .lean();
      const newOrder = ((maxOrderDoc as any)?.order ?? 0) + 1;

      category = await Category.create({
        name: mainCatName,
        icon: "📁",
        parent_id: null,
        level: 0,
        subcategories: [],
        order: newOrder,
        isActive: true,
      });

      console.log(`  ✅  Created: "${mainCatName}"`);
      stats.created++;
    } else {
      console.log(`  ♻️   Exists : "${category.name}" — merging sub-categories…`);
      stats.alreadyExisted++;
    }

    // Merge subcategories (skip duplicates)
    const existingSubsLower = new Set(
      (category.subcategories as string[]).map((s: string) => s.toLowerCase())
    );
    const newSubs = subcategories.filter(
      (s) => !existingSubsLower.has(s.toLowerCase())
    );

    if (newSubs.length > 0) {
      (category.subcategories as string[]).push(...newSubs);
      await category.save();
      stats.subsAdded += newSubs.length;
      newSubs.forEach((s) => console.log(`      └─ Added sub: "${s}"`));
    } else if (subcategories.length > 0) {
      console.log(
        `      └─ All ${subcategories.length} sub-categor${subcategories.length !== 1 ? "ies" : "y"} already exist — skipped.`
      );
      stats.subsSkipped += subcategories.length;
    }

    // Also create child nodes in the hierarchy tree (level 1 nodes)
    for (const subName of newSubs) {
      const exists = await Category.findOne({
        parent_id: category._id,
        name: {
          $regex: new RegExp(
            `^${subName.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}$`,
            "i"
          ),
        },
      });

      if (!exists) {
        const maxSubOrder = await Category.findOne({ parent_id: category._id })
          .sort({ order: -1 })
          .lean();
        await Category.create({
          name: subName,
          icon: "📌",
          parent_id: category._id,
          level: 1,
          order: ((maxSubOrder as any)?.order ?? 0) + 1,
          isActive: true,
        });
      }
    }
  }

  console.log();
  console.log("─────────────────────────────────────────────────");
  console.log("🎉  Upload complete!");
  console.log(`    Main categories created      : ${stats.created}`);
  console.log(`    Main categories already exist: ${stats.alreadyExisted}`);
  console.log(`    Sub-categories added         : ${stats.subsAdded}`);
  console.log(`    Sub-categories already exist : ${stats.subsSkipped}`);
  console.log("─────────────────────────────────────────────────");

  await mongoose.connection.close();
}

main().catch((err) => {
  console.error("❌  Fatal error:", err);
  process.exit(1);
});
