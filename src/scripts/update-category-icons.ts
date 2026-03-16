import dotenv from "dotenv";
import path from "path";
import mongoose from "mongoose";
import Category from "../models/Category";

dotenv.config({ path: path.join(__dirname, "../../.env") });

type IconUpdate = {
  name: string;
  icon: string;
};

const ICON_UPDATES: IconUpdate[] = [
  { name: "Laptop Repairs", icon: "💻🔧" },
  { name: "Mobile & Internet Services", icon: "📱🌐" },
  { name: "Refrigerator Repairs", icon: "🧊🔧" },
  { name: "Appliance Repairs", icon: "🔌🔧" },
  { name: "Computer Training Institutes", icon: "💻🎓" },
  { name: "Website & App Development", icon: "💻📱" },
  { name: "Cake Shops & Bakeries", icon: "🎂🧁" },
  { name: "Daily Needs Stores", icon: "🛒🏪" },
  { name: "Groceries", icon: "🥦🛒" },
  { name: "Florists", icon: "💐" },
  { name: "Restaurants", icon: "🍽️" },
  { name: "Food Delivery Services", icon: "🛵🍔" },
  { name: "Online Food Ordering", icon: "📱🍔" },
  { name: "Foreign Exchange Services", icon: "💱💵" },
  { name: "Furniture Stores", icon: "🛋️" },
  { name: "Wallpapers & Home Decor", icon: "🖼️🏡" },
  { name: "Water Suppliers", icon: "💧🚰" },
  { name: "Medical Stores & Pharmacies", icon: "💊🏥" },
  { name: "Optical Stores", icon: "👓" },
  { name: "Pet Shops", icon: "🐶🐾" },
  { name: "Pet Care Services", icon: "🐾🩺" },
  { name: "Online Shopping", icon: "🛍️📦" },
  { name: "T-Shirt Printing", icon: "👕🖨️" },
  { name: "Bus on Hire", icon: "🚌" },
  { name: "Car & Cab Rentals", icon: "🚗" },
  { name: "Generators on Hire", icon: "⚡🔌" },
  { name: "Equipment Rentals", icon: "🛠️" },
  { name: "Tempos on Hire", icon: "🚚" },
  { name: "Astrologers", icon: "🔮" },
  { name: "Beauty Salons", icon: "💇‍♀️" },
  { name: "Bridal Makeup Artists", icon: "👰💄" },
  { name: "Makeup Artists", icon: "💄" },
  { name: "Dance Classes", icon: "💃" },
  { name: "Music Classes", icon: "🎵" },
  { name: "Fitness Centres", icon: "🏋️" },
  { name: "Gyms", icon: "💪" },
  { name: "Photographers & Videographers", icon: "📸🎥" },
  { name: "Tattoo Artists", icon: "🖋️" },
  { name: "Weight Loss Centres", icon: "⚖️" },
  { name: "Movies", icon: "🎬" },
  { name: "Online Movie Platforms", icon: "📺" },
  { name: "Parties & Nightlife", icon: "🎉🍸" },
  { name: "General Physicians", icon: "🩺" },
  { name: "General Surgeons", icon: "🏥🔪" },
  { name: "Cardiologists", icon: "❤️🩺" },
  { name: "Child Specialists", icon: "👶🩺" },
  { name: "Paediatricians", icon: "👶🏥" },
  { name: "Dentists", icon: "🦷" },
  { name: "Dermatologists", icon: "🧴" },
  { name: "Skin & Hair Specialists", icon: "💇‍♀️🧴" },
  { name: "ENT Doctors", icon: "👂👃" },
  { name: "Eye Specialists", icon: "👁️" },
  { name: "Ophthalmologists", icon: "👁️🩺" },
  { name: "Gastroenterologists", icon: "🫃" },
  { name: "Gynaecologists & Obstetricians", icon: "🤰" },
  { name: "Neurologists", icon: "🧠" },
  { name: "Orthopaedic Doctors", icon: "🦴" },
  { name: "Ayurvedic Doctors", icon: "🌿🩺" },
  { name: "Homeopathic Doctors", icon: "🌿💊" },
  { name: "Pathology Labs", icon: "🔬🧪" },
  { name: "Physiotherapists", icon: "🦵" },
  { name: "Vaccination Centres", icon: "💉" },
  { name: "Hearing Aids & Solutions", icon: "🦻" },
  { name: "Schools & Educational Institutions", icon: "🏫" },
  { name: "Playgroups", icon: "🧸" },
  { name: "Kindergartens", icon: "🎨" },
  { name: "Home Tutors", icon: "📚" },
  { name: "Tutorials & Coaching Classes", icon: "📖" },
  { name: "Training Institutes", icon: "🎓" },
  { name: "Language Classes", icon: "🗣️" },
  { name: "Motor Training Schools", icon: "🚗🎓" },
  { name: "Overseas Education Consultants", icon: "🌍🎓" },
  { name: "Yoga & Wellness Classes", icon: "🧘‍♂️🌿" },
  { name: "Borewell Contractors", icon: "🚰" },
  { name: "Builders & Contractors", icon: "🏗️" },
  { name: "Carpentry Contractors", icon: "🪚" },
  { name: "Civil Contractors", icon: "🏗️" },
  { name: "Electrical Contractors", icon: "⚡" },
  { name: "Electricians", icon: "⚡🔧" },
  { name: "False Ceiling Contractors", icon: "🏠" },
  { name: "Home Services", icon: "🏠🔧" },
  { name: "Housekeeping Services", icon: "🧹" },
  { name: "Modular Kitchen Designers", icon: "🍳🏠" },
  { name: "Painting Contractors", icon: "🎨" },
  { name: "Plumbers", icon: "🚰🔧" },
  { name: "Ready Mix Concrete Suppliers", icon: "🏗️🚚" },
  { name: "Waterproofing Contractors", icon: "💧🏠" },
  { name: "Automobile Dealers", icon: "🚗🏪" },
  { name: "Car Insurance Agents", icon: "🚗📄" },
  { name: "Car Loans & Finance", icon: "🚗💰" },
  { name: "Car Repairs & Services", icon: "🚗🔧" },
  { name: "Taxi & Cab Services", icon: "🚕" },
  { name: "Towing Services", icon: "🚚" },
  { name: "Transporters & Logistics", icon: "🚛📦" },
  { name: "Chartered Accountants", icon: "📊" },
  { name: "Business Consultants", icon: "💼" },
  { name: "GST Registration Consultants", icon: "🧾" },
  { name: "Income Tax Consultants", icon: "💰📄" },
  { name: "Registration Consultants", icon: "📑" },
  { name: "Event Organizers", icon: "🎉" },
  { name: "Party Organisers", icon: "🎊" },
  { name: "Wedding Planners & Requisites", icon: "💍" },
  { name: "Interior Designers", icon: "🏠🎨" },
  { name: "Lawyers & Legal Services", icon: "⚖️" },
  { name: "Logistics & Supply Chain", icon: "🚚📦" },
  { name: "Online Passport Agents", icon: "🛂" },
  { name: "Packers & Movers", icon: "📦🚚" },
  { name: "Repairs & Maintenance Services", icon: "🛠️" },
  { name: "Website Designers & Developers", icon: "💻🌐" },
  { name: "Courier Services", icon: "📦" },
  { name: "Pest Control", icon: "🐜🚫" },
  { name: "Security Services", icon: "🛡️" },
  { name: "Cleaning Services", icon: "🧹" },
  { name: "Laundry & Dry Cleaning", icon: "👕🧺" },
  { name: "Catering Services", icon: "🍽️" },
  { name: "Photography Services", icon: "📸" },
  { name: "Printing Services", icon: "🖨️" },
  { name: "AC Repair & Services", icon: "❄️🔧" },
  { name: "Appliance Installation", icon: "🔌🛠️" },
  { name: "Buy", icon: "🛒" },
  { name: "Post Free Property Ad", icon: "🏠📢" },
];

function normalizeName(value: unknown): string {
  return String(value ?? "").trim().toLowerCase();
}

function parseArgs(argv: string[]) {
  return {
    dryRun: argv.includes("--dry-run"),
  };
}

async function main() {
  const { dryRun } = parseArgs(process.argv.slice(2));
  const mongoUri = process.env.MONGODB_URI || process.env.MONGO_URI;

  if (!mongoUri) {
    throw new Error("MONGODB_URI (or MONGO_URI) is not configured");
  }

  await mongoose.connect(mongoUri);
  console.log("[UPDATE-CATEGORY-ICONS] Connected to MongoDB");
  console.log("[UPDATE-CATEGORY-ICONS] Mode:", dryRun ? "DRY RUN" : "WRITE");

  const iconByName = new Map<string, string>();
  for (const item of ICON_UPDATES) {
    const key = normalizeName(item.name);
    if (!key) continue;
    iconByName.set(key, item.icon);
  }

  const nameList = Array.from(
    new Set(ICON_UPDATES.map((item) => item.name.trim()).filter(Boolean)),
  );

  const docs = await Category.find({ name: { $in: nameList } })
    .select("_id name icon parent_id level")
    .lean();

  let unchanged = 0;
  let updates = 0;
  const ops: any[] = [];

  for (const doc of docs) {
    const key = normalizeName((doc as any).name);
    const targetIcon = iconByName.get(key);
    if (!targetIcon) continue;

    if (String((doc as any).icon || "") === targetIcon) {
      unchanged += 1;
      continue;
    }

    updates += 1;
    if (dryRun) {
      console.log("[UPDATE-CATEGORY-ICONS][DRY-RUN] Would update", {
        id: String((doc as any)._id),
        name: (doc as any).name,
        from: (doc as any).icon || "",
        to: targetIcon,
        parent_id: (doc as any).parent_id ? String((doc as any).parent_id) : null,
        level: (doc as any).level ?? null,
      });
    } else {
      ops.push({
        updateOne: {
          filter: { _id: (doc as any)._id },
          update: { $set: { icon: targetIcon } },
        },
      });
    }
  }

  if (!dryRun && ops.length > 0) {
    const result = await Category.bulkWrite(ops, { ordered: false });
    console.log("[UPDATE-CATEGORY-ICONS] Bulk write result:", {
      matchedCount: result.matchedCount,
      modifiedCount: result.modifiedCount,
      upsertedCount: result.upsertedCount,
    });
  }

  const foundNameSet = new Set(docs.map((doc) => normalizeName((doc as any).name)));
  const missingNames = ICON_UPDATES.filter((item) => !foundNameSet.has(normalizeName(item.name))).map(
    (item) => item.name,
  );

  console.log("[UPDATE-CATEGORY-ICONS] Summary:", {
    configuredNames: ICON_UPDATES.length,
    matchedDocuments: docs.length,
    updates,
    unchanged,
    missingNameCount: missingNames.length,
  });

  if (missingNames.length > 0) {
    console.log("[UPDATE-CATEGORY-ICONS] Names not found in DB:");
    for (const name of missingNames) {
      console.log(" -", name);
    }
  }
}

main()
  .then(async () => {
    if (mongoose.connection.readyState !== 0) {
      await mongoose.connection.close();
    }
    console.log("[UPDATE-CATEGORY-ICONS] Completed");
    process.exit(0);
  })
  .catch(async (error: any) => {
    console.error("[UPDATE-CATEGORY-ICONS] Failed:", error?.message || error);
    if (mongoose.connection.readyState !== 0) {
      await mongoose.connection.close();
    }
    process.exit(1);
  });
