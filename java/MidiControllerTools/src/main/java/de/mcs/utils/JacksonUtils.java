package de.mcs.utils;
/**
 * 
 */

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;

/**
 * @author wklaa_000
 *
 */
public class JacksonUtils {

  private static ObjectMapper ymlObjectMapper;

  public static ObjectMapper getYmlMapper() {
    if (ymlObjectMapper == null) {
      ymlObjectMapper = new ObjectMapper(new YAMLFactory());
      ymlObjectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
      ymlObjectMapper.configure(SerializationFeature.WRITE_NULL_MAP_VALUES, false);
      ymlObjectMapper.setSerializationInclusion(Include.NON_NULL);
    }
    return ymlObjectMapper;
  }

  private static ObjectMapper jsonObjectMapper;

  public static ObjectMapper getJsonMapper() {
    if (jsonObjectMapper == null) {
      jsonObjectMapper = new ObjectMapper();
      jsonObjectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
      jsonObjectMapper.configure(SerializationFeature.WRITE_NULL_MAP_VALUES, false);
      jsonObjectMapper.setSerializationInclusion(Include.NON_NULL);
    }
    return jsonObjectMapper;
  }

  private static class StringMessage {
    @JsonProperty
    private String message;

    public StringMessage(String message) {
      this.message = message;
    }
  }

  public static String messageToJson(String message) throws JsonProcessingException {
    return getJsonMapper().writeValueAsString(new StringMessage(message));
  }
}
