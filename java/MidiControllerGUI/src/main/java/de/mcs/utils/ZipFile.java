/**
 * 
 */
package de.mcs.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * @author w.klaas
 */
public class ZipFile implements AutoCloseable {

  private ZipOutputStream zip;

  /**
   * Creating a new zip file. Adding files or folders to it using {@link #addToZip(String, File)} After all files added
   * use {@link #close()} to close the zip file.
   * 
   * @param zipFile
   *          the new zip file to create
   * @throws IOException
   *           if something goes wrong.
   */
  public ZipFile(File zipFile) throws IOException {
    if (zipFile.exists()) {
      throw new FileAlreadyExistsException(zipFile.toString());
    }
    zip = null;
    FileOutputStream fileWriter = null;
    if (!zipFile.getParentFile().exists()) {
      zipFile.getParentFile().mkdirs();
    }
    fileWriter = new FileOutputStream(zipFile);
    zip = new ZipOutputStream(fileWriter);
  }

  /**
   * Write the content of srcFile in a new ZipEntry, named path+srcFile, of the zip stream. The result is that the
   * srcFile will be in the path folder in the generated archive.
   * 
   * @param path
   *          String, the relative path with the root archive.
   * @param source
   *          String, the absolute path of the file to add
   */
  public void addToZip(final String path, final File source) {

    if (source.isDirectory()) {
      if (path.equals("")) {
        addFolderToZip(source.getName(), source);
      } else {
        addFolderToZip(path + "/" + source.getName(), source);
      }
    } else {
      // Transfer bytes from in to out
      byte[] buf = new byte[1024];
      int len;
      try {
        FileInputStream in = new FileInputStream(source);
        try {
          String zipEntryPrefix = "";
          if (!path.equals("")) {
            zipEntryPrefix = path + "/";
          }
          ZipEntry zipEntry = new ZipEntry(zipEntryPrefix + source.getName());
          zipEntry.setTime(source.lastModified());
          zip.putNextEntry(zipEntry);
          while ((len = in.read(buf)) > 0) {
            zip.write(buf, 0, len);
          }
        } finally {
          in.close();
        }
      } catch (Exception ex) {
        ex.printStackTrace();
      }
    }
  }

  /**
   * add the srcFolder to the zip stream.
   * 
   * @param path
   *          String, the relatif path with the root archive.
   * @param srcFolder
   *          String, the absolute path of the file to add
   */
  private void addFolderToZip(final String path, final File srcFolder) {
    File[] fileListe = srcFolder.listFiles();
    for (File file : fileListe) {
      addToZip(path, file);
    }
  }

  /**
   * closing the zip file.
   */
  @Override
  public void close() throws Exception {
    zip.flush();
    zip.close();
  }
}
