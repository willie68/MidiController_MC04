/**
 * 
 */
package de.mcs.utils;

import java.io.IOException;
import java.io.OutputStream;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;

/**
 * @author w.klaas
 *
 */
public class JacksonUtils {

  private static ObjectMapper ymlObjectMapper;

  /**
   * getting an jackson object mapper for read/writing yml files 
   * @return the desired object mapper
   */
  public static ObjectMapper getYmlMapper() {
    if (ymlObjectMapper == null) {
      ymlObjectMapper = new ObjectMapper(new YAMLFactory());
      ymlObjectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
      ymlObjectMapper.configure(SerializationFeature.WRITE_NULL_MAP_VALUES, false);
      ymlObjectMapper.setSerializationInclusion(Include.NON_NULL);
    }
    return ymlObjectMapper;
  }

  /**
   * this is a workaround for older jackson-databind. BEcause of an bug the yml factory can't write yml files.
   *  
   * @param object the object to write out
   * @param fos the file output stream to write the yaml structures
   * @throws JsonProcessingException if something goes worng.
   * @throws IOException if something goes worng.
   */
  public static void writeYAMLObject(Object object, OutputStream fos) throws JsonProcessingException, IOException {
    YAMLFactory yf = new YAMLFactory();
    yf.createGenerator(fos).writeObject(object);
  }

  private static ObjectMapper jsonObjectMapper;

  /**
   * getting an jackson object mapper for read/writing json files 
   * @return the desired object mapper
   */
  public static ObjectMapper getJsonMapper() {
    if (jsonObjectMapper == null) {
      jsonObjectMapper = new ObjectMapper();
      jsonObjectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
      jsonObjectMapper.configure(SerializationFeature.WRITE_NULL_MAP_VALUES, false);
      jsonObjectMapper.setSerializationInclusion(Include.NON_NULL);
    }
    return jsonObjectMapper;
  }
}
