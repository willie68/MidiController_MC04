/**
 * 
 */
package de.mcs.utils;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * @author w.klaas
 *
 */
public class Hasher {

  private static MessageDigest getMessageDigest() throws NoSuchAlgorithmException {
    return MessageDigest.getInstance("SHA-512");
  }

  public static String hashBytes(byte[] data) throws IOException, NoSuchAlgorithmException {
    try (BufferedInputStream stream = new BufferedInputStream(new ByteArrayInputStream(data));) {
      return Hasher.hash(stream);
    }
  }

  /**
   * Hashes the given BufferedInputStream with the specified algorithm type.
   * 
   * @param type The algorithm type.
   * @param input The input stream to read.
   * @return The hex string from the hash result.
   * @throws IOException On failure!
   * @throws NoSuchAlgorithmException 
   */
  public static String hash(BufferedInputStream input) throws IOException, NoSuchAlgorithmException {
    return toHexString(hash(getMessageDigest(), input));
  }

  private static byte[] hash(MessageDigest digest, BufferedInputStream input) throws IOException {
    byte[] dataBytes = new byte[8 * 4096];
    int nread = 0;
    while ((nread = input.read(dataBytes)) != -1)
      digest.update(dataBytes, 0, nread);
    return digest.digest();
  }

  static final char[] HEX_CHAR_ARRAY = "0123456789ABCDEF".toCharArray();

  public static String toHexString(final byte[] block) {
    StringBuilder sb = new StringBuilder(block.length * 2);
    for (byte b : block) {
      sb.append(HEX_CHAR_ARRAY[(b & 0xF0) >> 4]);
      sb.append(HEX_CHAR_ARRAY[b & 0x0F]);
    }
    return sb.toString();
  }
}
