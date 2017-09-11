package com.lmax.undertaker.junit;

import com.lmax.undertaker.junit.generators.ByteGen;
import org.junit.*;

import java.time.Instant;
import java.util.Date;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

public class SourceRuleTest {
    private int clearedBefore;

    @Rule
    public Source source = new SourceRule();

    @Before
    public void before()
    {
        clearedBefore = 0;
    }

    @Test
    public void beforeRunsWithEveryIteration()
    {
        Assert.assertEquals(0, clearedBefore);
        clearedBefore = source.getInt();
    }

    @Test
    public void compilesAndRuns()
    {
        System.out.println(new long[1].getClass().getName());
    }

    @Test
    public void canGetAnInt()
    {
        int anInt = source.getInt();
        Assert.assertTrue(anInt >= Integer.MIN_VALUE);
        Assert.assertTrue(anInt <= Integer.MAX_VALUE);
    }

    @Test
    public void canGetIntInRange()
    {
        int anInt = source.getInt(1, 10);

        Assert.assertTrue(anInt >= 1);
        Assert.assertTrue(anInt<= 10);
    }

    @Test
    public void canGetAnBetweenMaxAndMin()
    {
        int anInt = source.getInt(0, 1);
        Assert.assertTrue(anInt == 0 || anInt == 1);
    }

    @Test
    @Ignore
    public void canFail()
    {
        Assert.assertTrue(false);
    }

    @Test
    @Ignore
    public void canFailWithNiceishMessageWhenUsingAGenerator()
    {
        Assert.assertTrue(source.getBool());
    }

    @Test
    public void canGetABoolean()
    {
        final boolean bool = source.getBool();
        Assert.assertTrue(bool || !bool);
    }

    @Test
    public void canGetAByte()
    {
        final byte aByte = source.getByte();
        Assert.assertTrue(aByte >= Byte.MIN_VALUE);
        Assert.assertTrue(aByte <= Byte.MAX_VALUE);
    }

    @Test
    public void canGetAList() throws Exception
    {
        final List<Date> list = source.getList(SourceRuleTest::generateDate);
        Assert.assertTrue(list != null);
        final List<Byte> aListAOfBytes = source.getList(ByteGen::getByte);
        Assert.assertTrue(aListAOfBytes != null);
    }

    @Test
    public void canGetAShort() throws Exception
    {
        final short aShort = source.getShort();
        Assert.assertTrue(aShort >= Short.MIN_VALUE);
        Assert.assertTrue(aShort <= Short.MAX_VALUE);
    }

    @Test
    public void canGetAFloat() throws Exception
    {
        final float aFloat = source.getFloat();
        if (Double.isFinite(aFloat) && !Double.isNaN(aFloat)) {
            Assert.assertTrue(aFloat >= -Float.MAX_VALUE);
            Assert.assertTrue(aFloat <= Float.MAX_VALUE);
        }
    }

    @Test
    public void canGetADouble() throws Exception
    {
        final double aDouble = source.getDouble();
        if (Double.isFinite(aDouble) && !Double.isNaN(aDouble)) {
            Assert.assertTrue(aDouble >= -Double.MAX_VALUE);
            Assert.assertTrue(aDouble <= Double.MAX_VALUE);
        }
    }

    @Test
    public void canGetRealDouble() throws Exception
    {
        final double realDouble = source.getRealDouble();

        Assert.assertFalse(Double.isInfinite(realDouble));
        Assert.assertFalse(Double.isNaN(realDouble));

        Assert.assertTrue(realDouble >= -Double.MAX_VALUE);
        Assert.assertTrue(realDouble <= Double.MAX_VALUE);
    }

    @Test
    public void canGetALong() throws Exception
    {
        final long aLong = source.getLong();
        Assert.assertTrue(aLong >= Long.MIN_VALUE);
        Assert.assertTrue(aLong <= Long.MAX_VALUE);
    }

    @Test
    public void canGetLongArray() throws Exception
    {
        final long[] someLongs = source.getLongArray();
        for (long aLong : someLongs) {
            Assert.assertTrue(aLong >= Long.MIN_VALUE);
            Assert.assertTrue(aLong <= Long.MAX_VALUE);
        }
    }

    @Test
    public void canGetEveryKindOfChar() throws Exception
    {
        final char c = source.getChar();
        final char ascii = source.getAsciiChar();

        final char alpha = source.getAlphaChar();
        Assert.assertTrue(Character.isAlphabetic(alpha));

        final char alphaNum = source.getAlphanumericChar();
        Assert.assertTrue(Character.isAlphabetic(alphaNum) || Character.isDigit(alphaNum));
    }

    @Test
    public void canGetEveryKindOfString() throws Exception
    {
        final String s = source.getString();
        final String ascii = source.getAsciiString();

        final String alpha = source.getAlphaString();
        for (char c : alpha.toCharArray()) {
            Assert.assertTrue(Character.isAlphabetic(c));
        }

        final String alphaNum = source.getAlphanumericString();
        for (char c : alpha.toCharArray()) {
            Assert.assertTrue(Character.isAlphabetic(c) || Character.isDigit(c));
        }
    }

    @Test
    @Seed(1234567)
    @Trials(1)
    public void annotationsWork() throws Exception
    {
        Assert.assertEquals(-9223372036854775808L, source.getLong());
    }

    public static <T, V> Supplier<V> bind(Function<T, V> f, T input)
    {
        return () -> f.apply(input);
    }

    public static Date generateDate(Source s)
    {
        final long intervalId = s.pushInterval("Date");
        final Date generatedValue = Date.from(Instant.ofEpochSecond(s.getInt()));
        s.popInterval(intervalId, generatedValue);

        return generatedValue;
    }
}
