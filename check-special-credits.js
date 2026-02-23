import mongoose from "mongoose";
import dotenv from "dotenv";
import { fileURLToPath } from "url";
import { dirname, join } from "path";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

dotenv.config({ path: join(__dirname, ".env") });

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
  },
  { timestamps: true },
);

const UserSchema = new mongoose.Schema(
  {
    name: String,
    phone: String,
    isVoucherAdmin: Boolean,
    specialCredits: {
      balance: Number,
      totalReceived: Number,
      totalSent: Number,
      availableSlots: Number,
      usedSlots: Number,
    },
    level: Number,
  },
  { timestamps: true, strict: false },
);

async function checkSpecialCredits() {
  try {
    console.log("Connecting to MongoDB...");
    await mongoose.connect(process.env.MONGODB_URI || "");
    console.log("✅ Connected\n");

    const User = mongoose.models.User || mongoose.model("User", UserSchema);
    const SpecialCredit =
      mongoose.models.SpecialCredit ||
      mongoose.model("SpecialCredit", SpecialCreditSchema);

    // Find Rajesh Modi
    const admin = await User.findOne({ phone: { $regex: "9867477227" } });

    if (!admin) {
      console.log("❌ Admin not found");
      process.exit(1);
    }

    console.log("👤 Admin User:");
    console.log(`   Name: ${admin.name}`);
    console.log(`   Phone: ${admin.phone}`);
    console.log(`   ID: ${admin._id}`);
    console.log(`   isVoucherAdmin: ${admin.isVoucherAdmin}`);
    console.log(`   Level: ${admin.level}`);
    console.log(`   Special Credits:`, admin.specialCredits);
    console.log("");

    // Find special credit slots
    const slots = await SpecialCredit.find({ ownerId: admin._id }).sort({
      slotNumber: 1,
    });

    console.log(`🎰 Special Credit Slots: ${slots.length} found`);
    console.log("");

    if (slots.length === 0) {
      console.log("⚠️ No slots found! Run: node init-admin-special-credits.js");
    } else {
      slots.forEach((slot) => {
        console.log(`   Slot ${slot.slotNumber}:`);
        console.log(`     Status: ${slot.status}`);
        console.log(`     Credits: ${slot.creditAmount.toLocaleString()}`);
        console.log(`     Level: ${slot.level}`);
        if (slot.recipientName) {
          console.log(
            `     Sent to: ${slot.recipientName} (${slot.recipientPhone})`,
          );
        }
        console.log("");
      });
    }

    console.log("\n" + "=".repeat(60));
    console.log("SUMMARY");
    console.log("=".repeat(60));
    console.log(
      `Admin initialized: ${admin.isVoucherAdmin === true ? "✅ YES" : "❌ NO"}`,
    );
    console.log(`Slots created: ${slots.length}/10`);
    console.log(
      `Available slots: ${admin.specialCredits?.availableSlots || 0}`,
    );
    console.log("=".repeat(60));
  } catch (error) {
    console.error("❌ Error:", error);
  } finally {
    await mongoose.disconnect();
  }
}

checkSpecialCredits();
