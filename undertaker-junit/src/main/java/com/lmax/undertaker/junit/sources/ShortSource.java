package com.lmax.undertaker.junit.sources;

public interface ShortSource {
    short getShort();
    short getShort(short max);
    short getShort(short min, short max);
}
