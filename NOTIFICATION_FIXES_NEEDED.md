# Push Notification Fixes Needed

## Current Issues
- Users only receive notifications when app is OPEN (Socket.IO works)
- NO push notifications when app is closed or in background
- Missing notification types

## Required Notification Types

### ✅ 1. Individual Messages (ALREADY WORKING)
- **File**: `src/routes/messages.ts` - `/send` endpoint  
- **Status**: ✅ IMPLEMENTED (lines 60-71)
- **Sends**: Push notification to receiver if they have pushToken

### ❌ 2. Group Messages (NOT WORKING)
- **File**: `src/routes/messages.ts` - `/send-group` endpoint
- **Status**: ❌ MISSING - Only uses Socket.IO, no push notifications
- **Fix Needed**: Add push notifications to offline group members
- **Location**: After line 174 (after Socket.IO broadcasting)

```typescript
// Add this after Socket.IO broadcasting in /send-group endpoint:
const onlineUserIds = new Set<string>();

// Track which users are online (already connected via Socket.IO)
if (socketIO) {
  for (const memberId of group.members) {
    const memberIdStr = memberId.toString();
    if (memberIdStr !== senderId) {
      const memberSockets = Array.from(socketIO.sockets.sockets.values())
        .filter((socket: any) => socket.userId === memberIdStr);
      
      if (memberSockets.length > 0) {
        onlineUserIds.add(memberIdStr); // Mark as online
      }
    }
  }
}

// Send push notifications to OFFLINE members only
const offlineMembers = await User.find({
  _id: { 
    $in: group.members.filter((id: any) => 
      id.toString() !== senderId && !onlineUserIds.has(id.toString())
    )
  },
  pushToken: { $exists: true, $ne: null, $ne: 'expo-go-local-mode' }
}).select('pushToken name');

for (const member of offlineMembers) {
  await sendGroupMessageNotification(
    member.pushToken!,
    group.name,
    sender.name,
    text,
    groupId,
    senderId
  );
}
```

### ❌ 3. User Joined from Contacts (NOT IMPLEMENTED)
- **File**: `src/routes/contacts.ts` - `/sync` endpoint
- **Status**: ❌ NOT IMPLEMENTED
- **Notification**: "{ContactName} from your contacts joined InstantllyCards"
- **When**: After a contact syncs and finds matches with app users

```typescript
// Add to /sync endpoint after finding app user matches:
for (const match of appUserMatches) {
  // Send notification to all contacts who have this user's number
  const usersWithThisContact = await Contact.find({
    phoneNumber: match.phoneNumber,
    userId: { $ne: match.appUserId }
  }).populate('userId', 'name pushToken');

  for (const contact of usersWithThisContact) {
    const user = contact.userId as any;
    if (user.pushToken && user.pushToken !== 'expo-go-local-mode') {
      await sendContactJoinedNotification(
        user.pushToken,
        match.name,
        match.phoneNumber,
        match.appUserId.toString()
      );
    }
  }
}
```

### ❌ 4. Card Shared (NOT IMPLEMENTED)
- **File**: `src/routes/cards.ts` - card sharing endpoint
- **Status**: ❌ NOT IMPLEMENTED  
- **Notification**: "{SenderName} has sent you a Card"
- **When**: When someone shares a business card

```typescript
// Add to card sharing endpoint:
if (receiver.pushToken && receiver.pushToken !== 'expo-go-local-mode') {
  await sendCardSharingNotification(
    receiver.pushToken,
    sender.name,
    card.companyName,
    senderId,
    cardId
  );
}
```

### ❌ 5. Added to Group (NOT IMPLEMENTED)
- **File**: `src/routes/groups.ts` - `/` (create) and `/add-members` endpoints
- **Status**: ❌ NOT IMPLEMENTED
- **Notification**: "{CreatorName} has added you to {GroupName} Group"
- **When**: User is added to a new group

```typescript
// Add to group creation and add-members endpoints:
for (const memberId of newMemberIds) {
  const member = await User.findById(memberId);
  if (member?.pushToken && member.pushToken !== 'expo-go-local-mode') {
    await sendGroupInviteNotification(
      member.pushToken,
      creator.name,
      groupName,
      groupId.toString(),
      creatorId
    );
  }
}
```

## Implementation Order

1. ✅ **DONE**: Add notification helper functions to `pushNotifications.ts`
   - `sendGroupInviteNotification()`
   - `sendContactJoinedNotification()`

2. **TODO**: Fix group messages in `src/routes/messages.ts`
   - Add offline member detection
   - Send push notifications to offline members only

3. **TODO**: Add group invite notifications in `src/routes/groups.ts`
   - In POST `/` (create group)
   - In POST `/:groupId/add-members`

4. **TODO**: Add card sharing notifications in `src/routes/cards.ts`
   - Find card sharing endpoint
   - Add push notification

5. **TODO**: Add contact joined notifications in `src/routes/contacts.ts`
   - In POST `/sync` endpoint
   - Notify existing users when their contact joins

## Testing Requirements

After implementing all fixes:

1. Build app with `eas build` (not Expo Go)
2. Test each notification type:
   - Close app completely
   - Trigger notification from another device
   - Verify notification appears
   - Tap notification and verify it opens correct screen

## Current Status
- ✅ Individual messages: Working
- ❌ Group messages: Not sending push notifications
- ❌ User joined: Not implemented
- ❌ Card shared: Not implemented  
- ❌ Group invite: Not implemented
