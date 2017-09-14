package com.lmax.undertaker.junit.sources;

import com.lmax.undertaker.junit.generators.IntGenerator;

public interface StringSource {
    String getString();
    String getString(IntGenerator codePointGenerator);
    String getString(IntGenerator codePointGenerator, int maxLength);
    String getString(IntGenerator codePointGenerator, int minLength, int maxLength);
}
