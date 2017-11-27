package net.lfn3.undertaker.junit.sources;

public interface IntSource {
    default int getInt()
    {
        return getInt(Integer.MIN_VALUE, Integer.MAX_VALUE);
    }

    default int getInt(int max)
    {
        return getInt(Integer.MIN_VALUE, max);
    }

    int getInt(int min, int max);

    int getInt(int min, int max, int... moreRanges);
}
