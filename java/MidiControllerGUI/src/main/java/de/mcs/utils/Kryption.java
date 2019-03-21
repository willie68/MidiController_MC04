/**
 * 
 */
package de.mcs.utils;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.GeneralSecurityException;
import java.util.Base64;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.PBEParameterSpec;

/**
 * @author wklaa_000
 */
public class Kryption {

  // private static final String ALGORITHM = "PBEWithMD5AndDES";
  // PBEWithMD5AndTripleDES
  // PBEWITHSHA1ANDDESEDE
  // PBEWithSHA1AndDESed
  private static final String ALGORITHM = "PBEWITHSHA1ANDDESEDE";

  /**
   * main method for external encryption of passwords. Needed for the setup.
   * 
   * @param args
   *          index 0 will be the password to encrypt
   */
  public static void main(String[] args) {
    if (args.length > 0) {
      try {
        System.out.println(Kryption.encrypt(args[0]));
        System.exit(0);
      } catch (UnsupportedEncodingException | GeneralSecurityException e) {
        e.printStackTrace();
        System.exit(-1);
      }
    }
    System.exit(-1);
  }

  private static final char[] PASSWORD = "urmn88356ksqwgsftlj0&".toCharArray();
  private static final byte[] SALT = { (byte) 0xde, (byte) 0x33, (byte) 0x10, (byte) 0x12, (byte) 0xde, (byte) 0x33,
      (byte) 0x10, (byte) 0x12, };

  /**
   * encrypt this string.
   * 
   * @param string
   *          the string to encrypt
   * @return the encrypted and mime coded string.
   * @throws GeneralSecurityException
   *           if something goes wrong.
   * @throws UnsupportedEncodingException
   *           if something goes wrong.
   */
  public static String encrypt(String string) throws GeneralSecurityException, UnsupportedEncodingException {
    SecretKeyFactory keyFactory = SecretKeyFactory.getInstance(ALGORITHM);
    SecretKey key = keyFactory.generateSecret(new PBEKeySpec(PASSWORD));
    Cipher pbeCipher = Cipher.getInstance(ALGORITHM);
    pbeCipher.init(Cipher.ENCRYPT_MODE, key, new PBEParameterSpec(SALT, 20));
    return base64Encode(pbeCipher.doFinal(string.getBytes("UTF-8")));
  }

  private static String base64Encode(byte[] bytes) {
    // NB: This class is internal, and you probably should use another impl
    return Base64.getEncoder().encodeToString(bytes);
  }

  /**
   * decrypt this string.
   * 
   * @param string
   *          the string to decrypt
   * @return the mime decoded and decrypted string.
   * @throws GeneralSecurityException
   *           if something goes wrong.
   * @throws IOException
   *           if something goes wrong.
   */
  public static String decrypt(String string) throws GeneralSecurityException, IOException {
    SecretKeyFactory keyFactory = SecretKeyFactory.getInstance(ALGORITHM);
    SecretKey key = keyFactory.generateSecret(new PBEKeySpec(PASSWORD));
    Cipher pbeCipher = Cipher.getInstance(ALGORITHM);
    pbeCipher.init(Cipher.DECRYPT_MODE, key, new PBEParameterSpec(SALT, 20));
    return new String(pbeCipher.doFinal(base64Decode(string)), "UTF-8");
  }

  private static byte[] base64Decode(String property) throws IOException {
    // NB: This class is internal, and you probably should use another impl
    return Base64.getDecoder().decode(property);
  }

}
