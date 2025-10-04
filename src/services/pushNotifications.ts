import { Expo, ExpoPushMessage, ExpoPushTicket } from 'expo-server-sdk';

// Create a new Expo SDK client
const expo = new Expo({
  accessToken: process.env.EXPO_ACCESS_TOKEN || undefined, // Optional: for higher rate limits
  useFcmV1: true, // Use FCM v1 API
});

// Log the status of EXPO_ACCESS_TOKEN
if (process.env.EXPO_ACCESS_TOKEN) {
  console.log('✅ EXPO_ACCESS_TOKEN is configured');
} else {
  console.warn('⚠️ EXPO_ACCESS_TOKEN is not set - push notifications may have rate limits');
}

export async function sendPushNotification(
  pushToken: string,
  title: string,
  body: string,
  data?: any
): Promise<boolean> {
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('📤 [PUSH] Sending push notification');
  console.log('  Token:', pushToken.substring(0, 20) + '...');
  console.log('  Title:', title);
  console.log('  Body:', body);
  console.log('  Data:', JSON.stringify(data, null, 2));
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');

  try {
    // Check if the push token is valid
    if (!Expo.isExpoPushToken(pushToken)) {
      console.error(`❌ [PUSH] Invalid Expo push token: ${pushToken}`);
      return false;
    }

    console.log('✅ [PUSH] Push token is valid');

    // Create the message
    const message: ExpoPushMessage = {
      to: pushToken,
      sound: 'default',
      title,
      body,
      data: data || {},
      badge: 1,
      priority: 'high',
      channelId: 'default',
    };

    console.log('📦 [PUSH] Message payload:', JSON.stringify(message, null, 2));

    // Send the notification
    console.log('🚀 [PUSH] Calling Expo push notification API...');
    const tickets: ExpoPushTicket[] = await expo.sendPushNotificationsAsync([message]);
    
    console.log('📬 [PUSH] Received response from Expo:', JSON.stringify(tickets, null, 2));

    // Check if the notification was sent successfully
    const ticket = tickets[0];
    if (ticket.status === 'error') {
      console.error(`❌ [PUSH] Error sending push notification: ${ticket.message}`);
      if (ticket.details?.error) {
        console.error('❌ [PUSH] Error details:', ticket.details.error);
      }
      return false;
    }

    console.log(`✅ [PUSH] Push notification sent successfully!`);
    console.log(`✅ [PUSH] Ticket ID: ${ticket.id}`);
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
    return true;
  } catch (error: any) {
    console.error('❌ [PUSH] Exception while sending push notification:');
    console.error('❌ [PUSH] Error message:', error.message);
    console.error('❌ [PUSH] Error stack:', error.stack);
    console.error('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
    return false;
  }
}

export async function sendMessageNotification(
  pushToken: string,
  title: string,
  body: string,
  data?: string | any
): Promise<boolean> {
  // If data is a string, parse it, otherwise use as-is
  let notificationData = data;
  if (typeof data === 'string') {
    try {
      notificationData = JSON.parse(data);
    } catch (error) {
      console.error('Failed to parse notification data:', error);
      notificationData = { rawData: data };
    }
  }

  return await sendPushNotification(
    pushToken,
    title,
    body,
    {
      ...notificationData,
      timestamp: new Date().toISOString()
    }
  );
}

export async function sendGroupMessageNotification(
  pushToken: string,
  groupName: string,
  senderName: string,
  messageText: string,
  groupId: string,
  senderId: string
): Promise<boolean> {
  return await sendPushNotification(
    pushToken,
    groupName,
    `${senderName}: ${messageText}`,
    {
      type: 'group_message',
      groupId,
      groupName,
      senderId,
      senderName,
      messageText,
      timestamp: new Date().toISOString()
    }
  );
}

export async function sendIndividualMessageNotification(
  pushToken: string,
  senderName: string,
  messageText: string,
  senderId: string
): Promise<boolean> {
  return await sendPushNotification(
    pushToken,
    `New message from ${senderName}`,
    messageText,
    {
      type: 'individual_message',
      senderId,
      senderName,
      messageText,
      timestamp: new Date().toISOString()
    }
  );
}

export async function sendCardSharingNotification(
  pushToken: string,
  senderName: string,
  cardTitle: string,
  senderId: string,
  cardId: string
): Promise<boolean> {
  return await sendPushNotification(
    pushToken,
    `${senderName} sent you a Card`,
    `Check out ${cardTitle}`,
    {
      type: 'card_shared',
      senderId,
      senderName,
      cardTitle,
      cardId,
      timestamp: new Date().toISOString()
    }
  );
}

export async function sendGroupInviteNotification(
  pushToken: string,
  inviterName: string,
  groupName: string,
  groupId: string,
  inviterId: string
): Promise<boolean> {
  return await sendPushNotification(
    pushToken,
    `Added to ${groupName}`,
    `${inviterName} has added you to ${groupName}`,
    {
      type: 'group_invite',
      groupId,
      groupName,
      inviterId,
      inviterName,
      timestamp: new Date().toISOString()
    }
  );
}

export async function sendContactJoinedNotification(
  pushToken: string,
  contactName: string,
  contactPhone: string,
  contactId: string
): Promise<boolean> {
  return await sendPushNotification(
    pushToken,
    `${contactName} joined InstantllyCards`,
    `${contactName} from your contacts is now on InstantllyCards`,
    {
      type: 'contact_joined',
      contactId,
      contactName,
      contactPhone,
      timestamp: new Date().toISOString()
    }
  );
}

export async function sendCardCreationNotification(
  pushToken: string,
  creatorName: string,
  cardTitle: string,
  cardId: string,
  creatorId: string
): Promise<boolean> {
  return await sendPushNotification(
    pushToken,
    `${creatorName} created a new card`,
    `${creatorName} created: ${cardTitle}`,
    {
      type: 'card_created',
      cardId,
      creatorId,
      creatorName,
      cardTitle,
      timestamp: new Date().toISOString()
    }
  );
}

export async function sendBulkPushNotifications(
  notifications: Array<{
    pushToken: string;
    title: string;
    body: string;
    data?: any;
  }>
): Promise<void> {
  try {
    const messages: ExpoPushMessage[] = notifications
      .filter(notif => Expo.isExpoPushToken(notif.pushToken))
      .map(notif => ({
        to: notif.pushToken,
        sound: 'default',
        title: notif.title,
        body: notif.body,
        data: notif.data || {},
        badge: 1,
      }));

    if (messages.length === 0) {
      console.log('No valid push tokens to send notifications to');
      return;
    }

    // Send notifications in chunks
    const chunks = expo.chunkPushNotifications(messages);
    
    for (const chunk of chunks) {
      try {
        const tickets = await expo.sendPushNotificationsAsync(chunk);
        console.log(`📱 Sent ${tickets.length} push notifications`);
        
        // Log any errors
        tickets.forEach((ticket, index) => {
          if (ticket.status === 'error') {
            console.error(`Error sending notification ${index}:`, ticket.message);
          }
        });
      } catch (error) {
        console.error('Error sending notification chunk:', error);
      }
    }
  } catch (error) {
    console.error('Error sending bulk push notifications:', error);
  }
}