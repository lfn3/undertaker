package com.lmax.undertaker.junit.generators;

public interface LongGen {
    default long getLong()
    {
        return getLong(Long.MIN_VALUE, Long.MAX_VALUE);
    }

    default long getLong(long max)
    {
        return getLong(Long.MIN_VALUE, max);
    }

    long getLong(long min, long max);
}
