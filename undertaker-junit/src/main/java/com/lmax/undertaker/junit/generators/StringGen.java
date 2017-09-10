package com.lmax.undertaker.junit.generators;

public interface StringGen {
    String getString();
    String getString(int maxLength);
    String getString(int minLength, int maxLength);

    String getAsciiString();
    String getAsciiString(int maxLength);
    String getAsciiString(int minLength, int maxLength);

    String getAlphaString();
    String getAlphaString(int maxLength);
    String getAlphaString(int minLength, int maxLength);

    String getAlphanumericString();
    String getAlphanumericString(int maxLength);
    String getAlphanumericString(int minLength, int maxLength);
}
