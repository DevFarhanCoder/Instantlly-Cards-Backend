import { Expo, ExpoPushMessage, ExpoPushTicket } from 'expo-server-sdk';

// Create a new Expo SDK client
const expo = new Expo({
  accessToken: process.env.EXPO_ACCESS_TOKEN, // Optional: for higher rate limits
  useFcmV1: true, // Use FCM v1 API
});

export async function sendPushNotification(
  pushToken: string,
  title: string,
  body: string,
  data?: any
): Promise<boolean> {
  try {
    // Check if the push token is valid
    if (!Expo.isExpoPushToken(pushToken)) {
      console.error(`Push token ${pushToken} is not a valid Expo push token`);
      return false;
    }

    // Create the message
    const message: ExpoPushMessage = {
      to: pushToken,
      sound: 'default',
      title,
      body,
      data: data || {},
      badge: 1,
    };

    // Send the notification
    const tickets: ExpoPushTicket[] = await expo.sendPushNotificationsAsync([message]);
    
    // Check if the notification was sent successfully
    const ticket = tickets[0];
    if (ticket.status === 'error') {
      console.error(`Error sending push notification: ${ticket.message}`);
      if (ticket.details?.error) {
        console.error('Error details:', ticket.details.error);
      }
      return false;
    }

    console.log(`✅ Push notification sent successfully to ${pushToken}`);
    return true;
  } catch (error) {
    console.error('Error sending push notification:', error);
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