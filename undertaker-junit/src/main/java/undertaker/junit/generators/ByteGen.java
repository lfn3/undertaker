package undertaker.junit.generators;

public interface ByteGen {
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
