package com.lmax.undertaker.junit.generators;

public class CodePoints {
    public static final IntGenerator ASCII = source -> source.getInt(32, 126);
    public static final IntGenerator ALPHA = source -> source.getInt(0, 127); //TODO: needs to have ranges...
    public static final IntGenerator ALPHANUMERIC = source -> source.getInt(0, 127);
}
