# 🚀 Credit System Deployment Checklist

## Pre-Deployment
- [ ] Review changes in `src/routes/credits.ts`
- [ ] Review `src/models/User.ts` (comment fix)
- [ ] Verify `ADMIN_SECRET_KEY` is set in production `.env`
- [ ] Backup database (optional but recommended)

## Deployment
- [ ] Commit changes: `git add . && git commit -m "Add credit migration system"`
- [ ] Push to production: `git push`
- [ ] Wait for deployment (~2-3 minutes)
- [ ] Verify backend is running

## Migration
- [ ] Edit `scripts/migrate-all-users.ps1`:
  - [ ] Update `API_BASE` to production URL
  - [ ] Update `ADMIN_KEY` with your secret key
- [ ] Run migration: `.\scripts\migrate-all-users.ps1`
- [ ] Type "MIGRATE" to confirm
- [ ] Wait for completion (~1-2 minutes for 1000 users)

## Verification
- [ ] Check migration output shows success
- [ ] Verify a few user accounts manually
- [ ] Test new user signup → Should get 200 credits
- [ ] Test referral system → Referrer should get +300 credits
- [ ] Check `/api/credits/config` endpoint

## Post-Migration
- [ ] Monitor backend logs for any errors
- [ ] Test app on real device
- [ ] Announce update to users (optional)
- [ ] Update app version in stores

## Rollback (If Needed)
If something goes wrong:
```bash
# Connect to MongoDB
mongo "your-connection-string"

# Restore from backup
db.users.updateMany({}, { $set: { credits: <old_value> } })
```

---

## Quick Commands

```bash
# View current credit distribution
db.users.aggregate([
  { $group: { _id: "$credits", count: { $sum: 1 } } },
  { $sort: { count: -1 } }
])

# Check how many users need migration
db.users.countDocuments({ credits: { $ne: 200 } })

# View credit config
db.creditconfig.findOne()
```

---

## Support Contacts
- Backend Developer: [Your Name]
- DevOps: [Team Contact]
- Emergency: [Support Email]

---

## Timeline
- **Deployment**: ~5 minutes
- **Migration**: ~2 minutes
- **Testing**: ~10 minutes
- **Total**: ~20 minutes

---

**Status**: ⏳ Ready for deployment
**Last Updated**: December 24, 2025
