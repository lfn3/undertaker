package com.lmax.undertaker.junit.sources;

import com.lmax.undertaker.junit.Source;
import com.lmax.undertaker.junit.primitive.functions.ToCharFunction;

import java.util.function.ToDoubleFunction;

public interface CharArraySource {
    default char[] getCharArray()
    {
        return getCharArray(CharSource::getChar);
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
