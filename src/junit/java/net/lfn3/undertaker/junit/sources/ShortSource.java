package net.lfn3.undertaker.junit.sources;

public interface ShortSource {
    short getShort();
    short getShort(short max);
    short getShort(short min, short max);
}
