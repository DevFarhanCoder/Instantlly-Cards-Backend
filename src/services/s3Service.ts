import { S3Client, PutObjectCommand, DeleteObjectCommand, GetObjectCommand } from '@aws-sdk/client-s3';
import { Upload } from '@aws-sdk/lib-storage';
import { getSignedUrl } from '@aws-sdk/s3-request-presigner';
import { Readable } from 'stream';

// Initialize S3 client
const s3Client = new S3Client({
  region: process.env.AWS_REGION || 'ap-south-1',
  credentials: {
    accessKeyId: process.env.AWS_ACCESS_KEY_ID!,
    secretAccessKey: process.env.AWS_SECRET_ACCESS_KEY!,
  },
});

const bucket = process.env.S3_BUCKET || 'instantlly-media-prod';
const cloudfrontHost = process.env.CLOUDFRONT_HOST || 'd1rjsfuv5lw0hw.cloudfront.net';

/**
 * Encode S3 key for use in CloudFront/HTTP URLs.
 * Encodes each path segment (especially + → %2B) while preserving /
 */
function encodeS3KeyForUrl(key: string): string {
  return key.split('/').map(segment => encodeURIComponent(segment)).join('/');
}

/**
 * Sanitize an existing CloudFront URL to ensure + is encoded as %2B
 */
export function sanitizeCloudFrontUrl(url: string): string {
  if (!url) return url;
  try {
    const urlObj = new URL(url);
    // Re-encode the pathname: split by /, encode each segment, rejoin
    urlObj.pathname = urlObj.pathname.split('/').map(segment => encodeURIComponent(decodeURIComponent(segment))).join('/');
    return urlObj.toString();
  } catch {
    // Fallback: just replace + with %2B
    return url.replace(/\+/g, '%2B');
  }
}

/**
 * Generate S3 key (file path) based on type and identifiers
 */
export function generateS3Key(
  type: 'design-request' | 'pending-ad' | 'approved-ad',
  identifiers: {
    userId?: string;
    phone?: string;
    requestId?: string;
    adId?: string;
  },
  filename: string
): string {
  const timestamp = Date.now();
  const sanitizedFilename = filename.replace(/[^a-zA-Z0-9._-]/g, '_');

  // Remove + and other special chars from phone numbers for clean S3 paths
  const sanitizePhone = (phone: string) => phone.replace(/[^a-zA-Z0-9]/g, '');

  switch (type) {
    case 'design-request':
      const userFolder = sanitizePhone(identifiers.phone || identifiers.userId || 'unknown');
      const requestFolder = identifiers.requestId || 'unknown';
      const mediaType = filename.match(/\.(mp4|mov|avi|webm)$/i) ? 'videos' : 'images';
      return `design-requests/${userFolder}/${requestFolder}/${mediaType}/${timestamp}_${sanitizedFilename}`;

    case 'pending-ad':
      const uploaderPhone = sanitizePhone(identifiers.phone || 'unknown');
      const pendingAdId = identifiers.adId || 'unknown';
      return `pending-ads/${uploaderPhone}/${pendingAdId}/${sanitizedFilename}`;

    case 'approved-ad':
      const approvedAdId = identifiers.adId || 'unknown';
      return `approved-ads/${approvedAdId}/${sanitizedFilename}`;

    default:
      return `misc/${timestamp}_${sanitizedFilename}`;
  }
}

/**
 * Upload file buffer to S3
 */
export async function uploadToS3(
  buffer: Buffer,
  key: string,
  contentType: string
): Promise<{ url: string; key: string }> {
  try {
    console.log(`📤 Uploading to S3: ${key} (${contentType})`);

    const upload = new Upload({
      client: s3Client,
      params: {
        Bucket: bucket,
        Key: key,
        Body: buffer,
        ContentType: contentType,
        CacheControl: 'max-age=31536000', // Cache for 1 year
      },
    });

    await upload.done();

    // Return CloudFront URL for fast delivery (encode path segments so + becomes %2B etc.)
    const cloudfrontUrl = `https://${cloudfrontHost}/${encodeS3KeyForUrl(key)}`;

    console.log(`✅ Uploaded successfully: ${cloudfrontUrl}`);

    return {
      url: cloudfrontUrl,
      key: key,
    };
  } catch (error) {
    console.error('❌ S3 upload error:', error);
    throw new Error(`Failed to upload to S3: ${error}`);
  }
}

/**
 * Upload file stream to S3 (for large files)
 */
export async function uploadStreamToS3(
  stream: Readable,
  key: string,
  contentType: string
): Promise<{ url: string; key: string }> {
  try {
    console.log(`📤 Uploading stream to S3: ${key} (${contentType})`);

    const upload = new Upload({
      client: s3Client,
      params: {
        Bucket: bucket,
        Key: key,
        Body: stream as any,
        ContentType: contentType,
        CacheControl: 'max-age=31536000',
      },
    });

    await upload.done();

    const cloudfrontUrl = `https://${cloudfrontHost}/${encodeS3KeyForUrl(key)}`;

    console.log(`✅ Stream uploaded successfully: ${cloudfrontUrl}`);

    return {
      url: cloudfrontUrl,
      key: key,
    };
  } catch (error) {
    console.error('❌ S3 stream upload error:', error);
    throw new Error(`Failed to upload stream to S3: ${error}`);
  }
}

/**
 * Delete file from S3
 */
export async function deleteFromS3(key: string): Promise<void> {
  try {
    console.log(`🗑️ Deleting from S3: ${key}`);

    const command = new DeleteObjectCommand({
      Bucket: bucket,
      Key: key,
    });

    await s3Client.send(command);

    console.log(`✅ Deleted successfully: ${key}`);
  } catch (error) {
    console.error('❌ S3 delete error:', error);
    throw new Error(`Failed to delete from S3: ${error}`);
  }
}

/**
 * Delete multiple files from S3
 */
export async function deleteMultipleFromS3(keys: string[]): Promise<void> {
  try {
    console.log(`🗑️ Deleting ${keys.length} files from S3`);

    await Promise.all(keys.map(key => deleteFromS3(key)));

    console.log(`✅ Deleted ${keys.length} files successfully`);
  } catch (error) {
    console.error('❌ S3 batch delete error:', error);
    throw new Error(`Failed to delete multiple files from S3: ${error}`);
  }
}

/**
 * Generate signed URL for private file access (if needed later)
 */
export async function getSignedS3Url(key: string, expiresIn: number = 3600): Promise<string> {
  try {
    const command = new GetObjectCommand({
      Bucket: bucket,
      Key: key,
    });

    const signedUrl = await getSignedUrl(s3Client, command, { expiresIn });

    return signedUrl;
  } catch (error) {
    console.error('❌ S3 signed URL error:', error);
    throw new Error(`Failed to generate signed URL: ${error}`);
  }
}

/**
 * Get CloudFront URL from S3 key (encodes path segments so + becomes %2B)
 */
export function getCloudfrontUrl(key: string): string {
  return `https://${cloudfrontHost}/${encodeS3KeyForUrl(key)}`;
}

/**
 * Copy file from one S3 location to another (for pending → approved)
 */
export async function copyWithinS3(sourceKey: string, destinationKey: string): Promise<{ url: string; key: string }> {
  try {
    console.log(`📋 Copying in S3: ${sourceKey} → ${destinationKey}`);

    // Get the source object
    const getCommand = new GetObjectCommand({
      Bucket: bucket,
      Key: sourceKey,
    });

    const sourceObject = await s3Client.send(getCommand);

    // Upload to destination
    const putCommand = new PutObjectCommand({
      Bucket: bucket,
      Key: destinationKey,
      Body: sourceObject.Body as any,
      ContentType: sourceObject.ContentType,
      CacheControl: 'max-age=31536000',
    });

    await s3Client.send(putCommand);

    const cloudfrontUrl = `https://${cloudfrontHost}/${destinationKey}`;

    console.log(`✅ Copied successfully: ${cloudfrontUrl}`);

    return {
      url: cloudfrontUrl,
      key: destinationKey,
    };
  } catch (error) {
    console.error('❌ S3 copy error:', error);
    throw new Error(`Failed to copy in S3: ${error}`);
  }
}

/**
 * Helper: Upload design request image/video
 */
export async function uploadDesignRequestMedia(
  buffer: Buffer,
  filename: string,
  userId: string,
  phone: string,
  requestId: string,
  contentType: string
): Promise<{ url: string; key: string }> {
  const key = generateS3Key('design-request', { userId, phone, requestId }, filename);
  return uploadToS3(buffer, key, contentType);
}

/**
 * Helper: Upload pending ad media
 */
export async function uploadPendingAdMedia(
  buffer: Buffer,
  filename: string,
  uploaderPhone: string,
  adId: string,
  contentType: string
): Promise<{ url: string; key: string }> {
  const key = generateS3Key('pending-ad', { phone: uploaderPhone, adId }, filename);
  return uploadToS3(buffer, key, contentType);
}

/**
 * Helper: Upload approved ad media
 */
export async function uploadApprovedAdMedia(
  buffer: Buffer,
  filename: string,
  adId: string,
  contentType: string
): Promise<{ url: string; key: string }> {
  const key = generateS3Key('approved-ad', { adId }, filename);
  return uploadToS3(buffer, key, contentType);
}

/**
 * Helper: Move pending ad to approved (copy files and delete old)
 */
export async function movePendingToApproved(
  pendingAdId: string,
  uploaderPhone: string,
  mediaFiles: Array<{ filename: string; type: 'bottom_image' | 'fullscreen_image' | 'bottom_video' | 'fullscreen_video' }>
): Promise<Array<{ type: string; url: string; key: string }>> {
  try {
    const results = [];

    for (const file of mediaFiles) {
      const sourceKey = generateS3Key('pending-ad', { phone: uploaderPhone, adId: pendingAdId }, file.filename);
      const destKey = generateS3Key('approved-ad', { adId: pendingAdId }, file.filename);

      const { url, key } = await copyWithinS3(sourceKey, destKey);

      results.push({
        type: file.type,
        url,
        key,
      });

      // Optionally delete the pending file after copying
      // await deleteFromS3(sourceKey);
    }

    return results;
  } catch (error) {
    console.error('❌ Failed to move pending to approved:', error);
    throw error;
  }
}

export default {
  generateS3Key,
  uploadToS3,
  uploadStreamToS3,
  deleteFromS3,
  deleteMultipleFromS3,
  getSignedS3Url,
  getCloudfrontUrl,
  copyWithinS3,
  uploadDesignRequestMedia,
  uploadPendingAdMedia,
  uploadApprovedAdMedia,
  movePendingToApproved,
  sanitizeCloudFrontUrl,
};
