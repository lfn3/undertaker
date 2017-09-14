package com.lmax.undertaker.junit.sources;

public interface ByteSource {
    default byte getByte()
    {
        return getByte(Byte.MIN_VALUE, Byte.MAX_VALUE);
    }

    default byte getByte(byte max)
    {
        return getByte(Byte.MIN_VALUE, max);
    }

    byte getByte(byte min, byte max);
}
