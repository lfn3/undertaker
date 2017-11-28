package net.lfn3.undertaker.junit.generators;

public class CodePoints {
    public static final IntGenerator ASCII = source -> source.getInt(32, 126);
    public static final IntGenerator ALPHANUMERIC = source -> source.getInt(48, 57, 65, 90, 97,122);;
    public static final IntGenerator ALPHA = source -> source.getInt(65, 90, 97,122);
}
