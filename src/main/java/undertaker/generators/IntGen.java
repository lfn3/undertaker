package undertaker.generators;

import undertaker.Source;

import java.nio.ByteBuffer;

public interface IntGen {
    default int getInt()
    {
        return getInt(Integer.MIN_VALUE, Integer.MAX_VALUE);
    }

    default int getInt(int max)
    {
        return getInt(Integer.MIN_VALUE, max);
    }

    int getInt(int min, int max);

    static int moveIntoRange(int i, int min, int max)
    {
        if (min == Integer.MIN_VALUE && max == Integer.MAX_VALUE)
        {
            return i;
        }
        else if (max < min)
        {
            throw new IllegalArgumentException("min should be less than max");
        }
        else if (max == min)
        {
            return min;
        }

        long range = max - min;
        long fixedOffset = Integer.MIN_VALUE;
        double divisor = (Integer.MAX_VALUE / range) - (Integer.MIN_VALUE / range);
        long adjustedNumber = i - fixedOffset;

        return (int)Math.round(min + adjustedNumber / divisor);
    }

    static int getRawInt(Source source)
    {
        return ByteBuffer.wrap(source.getBytes(4)).getInt();
    }
}
