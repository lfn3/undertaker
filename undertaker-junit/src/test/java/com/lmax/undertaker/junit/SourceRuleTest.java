package com.lmax.undertaker.junit;

import com.lmax.undertaker.junit.generators.ByteGen;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;

import java.time.Instant;
import java.util.Date;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

public class SourceRuleTest {
    @Rule
    public Source source = new SourceRule();

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
    @Seed(1234567)
    @Trials(1)
    public void annotationsWork() throws Exception
    {
        Assert.assertEquals(7320916221699310354L, source.getLong());
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