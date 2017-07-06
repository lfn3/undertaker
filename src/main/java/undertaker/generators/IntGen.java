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

        final boolean positive = i >= 0; //i.e. will be in the upper half of the range.
        double halfOfRange = max / 2 + min / 2;
        double divisor = ((Integer.MAX_VALUE / 2) - (Integer.MIN_VALUE / 2)) / halfOfRange;
        if (positive)
        {
            return (int)Math.round(max - (i / divisor));
        }
        else
        {
            return (int)Math.round(min + (i / divisor));
        }
    }

    static int getRawInt(Source source)
    {
        return ByteBuffer.wrap(source.getBytes(4)).getInt();
    }
}
