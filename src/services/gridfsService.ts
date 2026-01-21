import mongoose from "mongoose";
import { GridFSBucket, ObjectId } from "mongodb";
import { Readable } from "stream";

class GridFSService {
  private bucket: GridFSBucket | null = null;
  private isInitialized = false;

  /**
   * Initialize GridFS bucket
   * Must be called after MongoDB connection is established
   */
  initialize() {
    if (this.isInitialized) {
      return;
    }

    if (!mongoose.connection.db) {
      throw new Error("MongoDB connection not established");
    }

    // Create GridFS bucket for ad images
    this.bucket = new GridFSBucket(mongoose.connection.db, {
      bucketName: "adImages"
    });

    this.isInitialized = true;
    console.log("✅ GridFS bucket initialized for ad images");
  }

  /**
   * Upload base64 image to GridFS
   * @param base64Data - Base64 encoded image (with or without data:image prefix)
   * @param filename - Filename for the image
   * @param metadata - Optional metadata
   * @returns GridFS file ID
   */
  async uploadBase64(
    base64Data: string,
    filename: string,
    metadata?: Record<string, any>
  ): Promise<ObjectId> {
    if (!this.bucket) {
      throw new Error("GridFS bucket not initialized");
    }

    // Detect if it's a video or image from data URL
    const isVideo = base64Data.startsWith('data:video/');
    
    // Remove data:image/video prefix if present
    const base64Clean = base64Data.replace(/^data:(image|video)\/\w+;base64,/, "");

    // Convert base64 to buffer
    const buffer = Buffer.from(base64Clean, "base64");

    // Create readable stream from buffer
    const readStream = Readable.from(buffer);

    // Create upload stream
    const uploadStream = this.bucket.openUploadStream(filename, {
      metadata: {
        ...metadata,
        uploadedAt: new Date(),
        size: buffer.length
      }
    });

    return new Promise((resolve, reject) => {
      readStream
        .pipe(uploadStream)
        .on("error", reject)
        .on("finish", () => {
          console.log(`✅ Uploaded ${filename} to GridFS (${uploadStream.id})`);
          resolve(uploadStream.id as ObjectId);
        });
    });
  }

  /**
   * Download image from GridFS as base64
   * @param fileId - GridFS file ID
   * @returns Base64 encoded image with data URI prefix
   */
  async downloadAsBase64(fileId: string | ObjectId): Promise<string> {
    if (!this.bucket) {
      throw new Error("GridFS bucket not initialized");
    }

    const id = typeof fileId === "string" ? new ObjectId(fileId) : fileId;

    // Get file metadata first
    const files = await this.bucket.find({ _id: id }).toArray();
    if (files.length === 0) {
      throw new Error(`File not found: ${id}`);
    }

    const file = files[0];

    // Download file as buffer
    const downloadStream = this.bucket.openDownloadStream(id);

    return new Promise((resolve, reject) => {
      const chunks: Buffer[] = [];

      downloadStream
        .on("data", (chunk: Buffer) => chunks.push(chunk))
        .on("error", reject)
        .on("end", () => {
          const buffer = Buffer.concat(chunks);
          const base64 = buffer.toString("base64");
          
          // Determine content type from filename
          const contentType = this.getContentType(file.filename);
          const dataUri = `data:${contentType};base64,${base64}`;
          
          resolve(dataUri);
        });
    });
  }

  /**
   * Download image as buffer (for streaming responses)
   * @param fileId - GridFS file ID
   */
  async downloadAsBuffer(fileId: string | ObjectId): Promise<Buffer> {
    if (!this.bucket) {
      throw new Error("GridFS bucket not initialized");
    }

    const id = typeof fileId === "string" ? new ObjectId(fileId) : fileId;
    const downloadStream = this.bucket.openDownloadStream(id);

    return new Promise((resolve, reject) => {
      const chunks: Buffer[] = [];

      downloadStream
        .on("data", (chunk) => chunks.push(chunk))
        .on("error", reject)
        .on("end", () => resolve(Buffer.concat(chunks)));
    });
  }

  /**
   * Get download stream for efficient file transfer
   * @param fileId - GridFS file ID
   */
  getDownloadStream(fileId: string | ObjectId) {
    if (!this.bucket) {
      throw new Error("GridFS bucket not initialized");
    }

    const id = typeof fileId === "string" ? new ObjectId(fileId) : fileId;
    return this.bucket.openDownloadStream(id);
  }

  /**
   * Check if file exists in GridFS
   * @param fileId - GridFS file ID
   * @returns true if file exists, false otherwise
   */
  async fileExists(fileId: string | ObjectId): Promise<boolean> {
    if (!this.bucket) {
      throw new Error("GridFS bucket not initialized");
    }

    try {
      const id = typeof fileId === "string" ? new ObjectId(fileId) : fileId;
      const files = await this.bucket.find({ _id: id }).limit(1).toArray();
      return files.length > 0;
    } catch (error) {
      return false;
    }
  }

  /**
   * Delete file from GridFS
   * @param fileId - GridFS file ID
   */
  async deleteFile(fileId: string | ObjectId): Promise<void> {
    if (!this.bucket) {
      throw new Error("GridFS bucket not initialized");
    }

    const id = typeof fileId === "string" ? new ObjectId(fileId) : fileId;
    await this.bucket.delete(id);
    console.log(`🗑️ Deleted file from GridFS: ${id}`);
  }

  /**
   * Get file metadata
   * @param fileId - GridFS file ID
   */
  async getFileInfo(fileId: string | ObjectId) {
    if (!this.bucket) {
      throw new Error("GridFS bucket not initialized");
    }

    const id = typeof fileId === "string" ? new ObjectId(fileId) : fileId;
    const files = await this.bucket.find({ _id: id }).toArray();
    
    if (files.length === 0) {
      throw new Error(`File not found: ${id}`);
    }

    return files[0];
  }

  /**
   * Get content type from filename
   */
  private getContentType(filename: string): string {
    const ext = filename.split(".").pop()?.toLowerCase();
    const mimeTypes: Record<string, string> = {
      jpg: "image/jpeg",
      jpeg: "image/jpeg",
      png: "image/png",
      gif: "image/gif",
      webp: "image/webp",
      svg: "image/svg+xml"
    };
    return mimeTypes[ext || ""] || "image/jpeg";
  }
}

// Export singleton instance
export const gridfsService = new GridFSService();
