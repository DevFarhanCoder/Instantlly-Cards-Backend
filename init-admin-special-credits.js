// Initialize Special Credits for Rajesh Modi (Admin)
// Run this script once to set up the admin's special credit slots

import mongoose from "mongoose";
import dotenv from "dotenv";
import { fileURLToPath } from "url";
import { dirname, join } from "path";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

dotenv.config({ path: join(__dirname, ".env") });

// Define schemas inline
const UserSchema = new mongoose.Schema(
  {
    name: String,
    phone: String,
    level: Number,
    parentId: mongoose.Schema.Types.ObjectId,
    isVoucherAdmin: Boolean,
    specialCredits: {
      balance: Number,
      totalReceived: Number,
      totalSent: Number,
      availableSlots: Number,
      usedSlots: Number,
    },
  },
  { timestamps: true, strict: false },
);

const SpecialCreditSchema = new mongoose.Schema(
  {
    ownerId: mongoose.Schema.Types.ObjectId,
    slotNumber: Number,
    creditAmount: Number,
    status: String,
    recipientId: mongoose.Schema.Types.ObjectId,
    recipientName: String,
    recipientPhone: String,
    sentAt: Date,
    level: Number,
    sourceSlotId: mongoose.Schema.Types.ObjectId,
  },
  { timestamps: true },
);

// Simple implementation without importing models
async function initializeAdminSpecialCredits() {
  try {
    console.log("\n" + "=".repeat(60));
    console.log("SPECIAL CREDITS ADMIN INITIALIZATION SCRIPT");
    console.log("=".repeat(60));
    console.log("Usage:");
    console.log("  node init-admin-special-credits.js");
    console.log("  node init-admin-special-credits.js <USER_ID>");
    console.log("=".repeat(60) + "\n");

    // Connect to MongoDB
    console.log("Connecting to MongoDB...");
    await mongoose.connect(process.env.MONGODB_URI || "");
    console.log("✅ Connected to MongoDB");

    // Register models
    const User = mongoose.models.User || mongoose.model("User", UserSchema);
    const SpecialCredit =
      mongoose.models.SpecialCredit ||
      mongoose.model("SpecialCredit", SpecialCreditSchema);

    // Check if user ID was provided as command line argument
    const userIdArg = process.argv[2];
    let admin;

    if (userIdArg) {
      console.log(`\nSearching for user by ID: ${userIdArg}`);
      admin = await User.findById(userIdArg);

      if (!admin) {
        console.error(`❌ User not found with ID: ${userIdArg}`);
        process.exit(1);
      }
    } else {
      // Find admin by phone number - try multiple methods
      console.log("\nSearching for admin user...");

      // Method 1: Exact match with +91 prefix
      admin = await User.findOne({ phone: "+91 9867477227" });

      // Method 2: Without + prefix
      if (!admin) {
        console.log("Trying without + prefix...");
        admin = await User.findOne({ phone: "91 9867477227" });
      }

      // Method 3: Without country code
      if (!admin) {
        console.log("Trying without country code...");
        admin = await User.findOne({ phone: "9867477227" });
      }

      // Method 4: With spaces removed
      if (!admin) {
        console.log("Trying without spaces...");
        admin = await User.findOne({ phone: "919867477227" });
      }

      // Method 5: Regex search
      if (!admin) {
        console.log("Trying regex search...");
        admin = await User.findOne({
          phone: { $regex: "9867477227", $options: "i" },
        });
      }

      // Method 6: Search by name
      if (!admin) {
        console.log("Trying by name 'Rajesh Modi'...");
        admin = await User.findOne({
          name: { $regex: "Rajesh Modi", $options: "i" },
        });
      }
    }

    if (!admin) {
      console.error("\n❌ Admin user not found with any method!");

      // List all users to help debug
      console.log("\n📋 Listing all users in database (first 10):");
      const allUsers = await User.find({}).limit(10).select("name phone _id");

      if (allUsers.length === 0) {
        console.log("  No users found in database!");
      } else {
        allUsers.forEach((user, index) => {
          console.log(
            `  ${index + 1}. Name: "${user.name}" | Phone: "${user.phone}" | ID: ${user._id}`,
          );
        });

        console.log(
          "\n💡 Please run the script with one of the above user IDs:",
        );
        console.log("   node init-admin-special-credits.js <USER_ID>");
        console.log(
          "\n   OR update the phone number in the script to match exactly.",
        );
      }

      process.exit(1);
    }

    console.log(`\n✅ Found admin: ${admin.name} (${admin.phone})`);
    console.log(`Admin ID: ${admin._id}`);

    // Update admin properties
    admin.isVoucherAdmin = true;
    admin.level = 0;
    admin.parentId = null;

    // Initialize special credits
    if (!admin.specialCredits) {
      admin.specialCredits = {
        balance: 0,
        totalReceived: 0,
        totalSent: 0,
        availableSlots: 10,
        usedSlots: 0,
      };
    } else {
      admin.specialCredits.availableSlots = 10;
    }

    await admin.save();
    console.log("✅ Admin properties updated");

    // Check if slots already exist
    const existingSlots = await SpecialCredit.countDocuments({
      ownerId: admin._id,
    });

    if (existingSlots >= 10) {
      console.log(
        `\n✅ Admin already has ${existingSlots} special credit slots`,
      );
      console.log("No need to create new slots.");
    } else {
      console.log("\nCreating 10 special credit slots...");

      const adminCreditAmount = 14648436000; // 14,648,436,000 credits per slot

      for (let i = 1; i <= 10; i++) {
        await SpecialCredit.create({
          ownerId: admin._id,
          slotNumber: i,
          creditAmount: adminCreditAmount,
          status: "available",
          level: 0,
        });
        console.log(
          `  ✓ Created slot ${i} with ${adminCreditAmount.toLocaleString()} credits`,
        );
      }

      console.log("\n✅ All 10 slots created successfully!");
    }

    // Display summary
    console.log("\n" + "=".repeat(60));
    console.log("ADMIN SPECIAL CREDITS INITIALIZATION COMPLETE");
    console.log("=".repeat(60));
    console.log(`Name: ${admin.name}`);
    console.log(`Phone: ${admin.phone}`);
    console.log(`Level: ${admin.level}`);
    console.log(`Is Voucher Admin: ${admin.isVoucherAdmin}`);
    console.log(`Total Slots: 10`);
    console.log(`Credits per Slot: 14,648,436,000`);
    console.log(`Vouchers Figure: 122,070,300`);
    console.log("=".repeat(60));

    console.log(
      "\n✅ Setup complete! Admin can now send special credits to users.",
    );
  } catch (error) {
    console.error("❌ Error:", error);
  } finally {
    await mongoose.disconnect();
    console.log("\n🔌 Disconnected from MongoDB");
  }
}

// Run the initialization
initializeAdminSpecialCredits();
