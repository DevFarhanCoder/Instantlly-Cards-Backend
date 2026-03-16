import dotenv from "dotenv";
import path from "path";
import mongoose from "mongoose";
import Category from "../models/Category";

dotenv.config({ path: path.join(__dirname, "../../.env") });

function normalizeName(value: unknown): string {
  return String(value ?? "").trim().toLowerCase();
}

async function main() {
  const mongoUri = process.env.MONGODB_URI || process.env.MONGO_URI;
  if (!mongoUri) {
    throw new Error("MONGODB_URI (or MONGO_URI) is not configured");
  }

  await mongoose.connect(mongoUri);
  console.log("[FIX-CATEGORY-INDEXES] Connected to MongoDB");

  const collection = Category.collection;
  const indexes = await collection.indexes();
  console.log("[FIX-CATEGORY-INDEXES] Existing indexes:", indexes.map((idx) => idx.name));

  const legacyNameUnique = indexes.find(
    (idx: any) => idx.name === "name_1" && idx.unique === true,
  );

  if (legacyNameUnique) {
    await collection.dropIndex("name_1");
    console.log('[FIX-CATEGORY-INDEXES] Dropped legacy unique index "name_1"');
  } else {
    console.log('[FIX-CATEGORY-INDEXES] Legacy unique index "name_1" not found (already removed)');
  }

  // Detect duplicate sibling names before creating/enforcing parent-scoped unique index.
  const allDocs = await Category.find({})
    .select("_id name parent_id")
    .lean();

  const byScope = new Map<string, string[]>();
  for (const doc of allDocs) {
    const parentKey = doc.parent_id ? String(doc.parent_id) : "ROOT";
    const nameKey = normalizeName((doc as any).name);
    if (!nameKey) continue;
    const key = `${parentKey}::${nameKey}`;
    const ids = byScope.get(key) || [];
    ids.push(String(doc._id));
    byScope.set(key, ids);
  }

  const siblingDuplicates = Array.from(byScope.entries()).filter(([, ids]) => ids.length > 1);
  if (siblingDuplicates.length > 0) {
    console.error("[FIX-CATEGORY-INDEXES] Duplicate sibling names found. Resolve these first:");
    for (const [scopeKey, ids] of siblingDuplicates.slice(0, 20)) {
      console.error(`  ${scopeKey} -> ${ids.join(", ")}`);
    }
    throw new Error(
      `Found ${siblingDuplicates.length} duplicate sibling key(s). Aborting index creation.`,
    );
  }

  await collection.createIndex({ parent_id: 1, name: 1 }, { unique: true });
  console.log('[FIX-CATEGORY-INDEXES] Ensured unique index on { parent_id: 1, name: 1 }');

  await collection.createIndex({ parent_id: 1, order: 1 });
  await collection.createIndex({ isActive: 1, order: 1 });
  console.log("[FIX-CATEGORY-INDEXES] Ensured supporting indexes");

  const finalIndexes = await collection.indexes();
  console.log("[FIX-CATEGORY-INDEXES] Final indexes:", finalIndexes.map((idx) => idx.name));
}

main()
  .then(async () => {
    if (mongoose.connection.readyState !== 0) {
      await mongoose.connection.close();
    }
    console.log("[FIX-CATEGORY-INDEXES] Completed");
    process.exit(0);
  })
  .catch(async (error: any) => {
    console.error("[FIX-CATEGORY-INDEXES] Failed:", error?.message || error);
    if (mongoose.connection.readyState !== 0) {
      await mongoose.connection.close();
    }
    process.exit(1);
  });
