import mongoose from "mongoose";
import dotenv from "dotenv";
import jwt from "jsonwebtoken";
import { fileURLToPath } from "url";
import { dirname, join } from "path";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

dotenv.config({ path: join(__dirname, ".env") });

const UserSchema = new mongoose.Schema(
  {
    name: String,
    phone: String,
    isVoucherAdmin: Boolean,
    level: Number,
  },
  { timestamps: true, strict: false },
);

async function generateAdminToken() {
  try {
    console.log("Connecting to MongoDB...");
    await mongoose.connect(process.env.MONGODB_URI || "");
    console.log("✅ Connected\n");

    const User = mongoose.models.User || mongoose.model("User", UserSchema);

    // Find Rajesh Modi
    const admin = await User.findOne({ phone: { $regex: "9867477227" } });

    if (!admin) {
      console.log("❌ Admin not found");
      process.exit(1);
    }

    console.log("👤 Admin Found:");
    console.log(`   Name: ${admin.name}`);
    console.log(`   Phone: ${admin.phone}`);
    console.log(`   ID: ${admin._id}`);
    console.log(`   isVoucherAdmin: ${admin.isVoucherAdmin}`);
    console.log("");

    // Generate JWT token
    const JWT_SECRET = process.env.JWT_SECRET || "your-secret-key";
    const token = jwt.sign(
      {
        sub: admin._id.toString(),
        id: admin._id.toString(),
        phone: admin.phone,
        isVoucherAdmin: admin.isVoucherAdmin,
      },
      JWT_SECRET,
      { expiresIn: "90d" }, // 90 days validity
    );

    console.log("=".repeat(70));
    console.log("🔑 ADMIN LOGIN TOKEN (Valid for 90 days)");
    console.log("=".repeat(70));
    console.log(token);
    console.log("=".repeat(70));
    console.log("");
    console.log("📱 To use this token in your app:");
    console.log("1. Copy the token above");
    console.log("2. In your app, logout and login again with +919867477227");
    console.log("3. Or manually set this token in AsyncStorage");
    console.log("");
    console.log("🧪 Test this token:");
    console.log(
      `curl http://localhost:3001/api/mlm/special-credits/network \\`,
    );
    console.log(`  -H "Authorization: Bearer ${token.substring(0, 50)}..."`);
    console.log("");
  } catch (error) {
    console.error("❌ Error:", error);
  } finally {
    await mongoose.disconnect();
  }
}

generateAdminToken();
