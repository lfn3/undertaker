package net.lfn3.undertaker.junit.sources;

import net.lfn3.undertaker.junit.generators.IntGenerator;

public interface StringSource {
    String getString();
    String getString(IntGenerator codePointGenerator);
    String getString(IntGenerator codePointGenerator, int maxLength);
    String getString(IntGenerator codePointGenerator, int minLength, int maxLength);
}
