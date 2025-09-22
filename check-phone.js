// Script to check if phone number exists in database
const { connectDB } = require("./dist/db.js");
const User = require("./dist/models/User.js").default;

async function checkPhone() {
  try {
    await connectDB();
    
    const phones = ["+917378356287", "917378356287", "+91917378356287", "91917378356287"];
    
    for (const phone of phones) {
      const user = await User.findOne({ phone });
      if (user) {
        console.log(`Found user with phone ${phone}:`, {
          name: user.name,
          phone: user.phone,
          createdAt: user.createdAt
        });
      } else {
        console.log(`No user found with phone: ${phone}`);
      }
    }
    
    process.exit(0);
  } catch (error) {
    console.error("Error:", error);
    process.exit(1);
  }
}

checkPhone();