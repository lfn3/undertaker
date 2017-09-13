package com.lmax.undertaker.junit.generators;

public interface RepeaterGen {
    default void repeatedly(Runnable r)
    {
        repeatedly(r, 0, 64);
    }

    default void repeatedly(Runnable r, int max)
    {
        repeatedly(r, 0, max);
    }

    void repeatedly(Runnable r, int min, int max);
}
