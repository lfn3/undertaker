package com.lmax.undertaker.junit.generators;

import com.lmax.undertaker.junit.Source;

import java.util.function.ToDoubleFunction;

public interface CharArrayGen {
    default char[] getCharArray()
    {
        return getCharArray(CharGen::getChar);
    }
    default char[] getCharArray(ToCharFunction<Source> generator)
    {
        return getCharArray(generator, 0, 64);
    }
    default char[] getCharArray(ToCharFunction<Source> generator, int minSize)
    {
        return getCharArray(generator, minSize, minSize + 64);
    }
    char[] getCharArray(ToCharFunction<Source> generator, int minSize, int maxSize);
}
