package com.lmax.undertaker.junit;

import com.lmax.undertaker.junit.generators.CodePoints;
import com.lmax.undertaker.junit.sources.ByteSource;
import javafx.util.Pair;
import org.junit.*;

import java.time.Instant;
import java.time.LocalDateTime;
import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;

public class SourceRuleTest {
    private static final Generator<Date> DATE_GENERATOR = s -> Date.from(Instant.ofEpochSecond(s.getInt(0, Integer.MAX_VALUE)));
    private static final Generator<String> DATE_STRING_GENERATOR = Generator.asGenerator(DATE_GENERATOR.andThen(Date::toString));

    private int clearedBefore;
    private List<Long> aList = new ArrayList<>();
    private static final Map<Class, Generator> GENERATORS = new HashMap<>();

    static
    {
        GENERATORS.put(GeneratorMapTestClass.class, s -> new GeneratorMapTestClass("Hello!"));
    }

    @Rule
    public Source source = new SourceRule(GENERATORS);

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
    public void runsOnANewlyInstansiatedClass()
    {
        Assert.assertEquals(0, aList.size());
        aList.add(source.getLong());
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
        final List<Byte> aListAOfBytes = source.getList(ByteSource::getByte);
        Assert.assertTrue(aListAOfBytes != null);
    }

    @Test
    public void canGetAnArray() throws Exception
    {
        final Date[] anArray = source.getArray(Date.class, SourceRuleTest::generateDate);
        Assert.assertTrue(anArray != null);
        final byte[] aByteArray = source.getByteArray();
        Assert.assertTrue(aByteArray != null);
    }

    @Test
    public void canGenerateWithFunction() throws Exception
    {
        final Date generated = source.generate(SourceRuleTest::generateDate);
        Assert.assertNotNull(generated);

        final Date functionGenerated = source.generate(DATE_GENERATOR);
        Assert.assertNotNull(functionGenerated);

        final String composedFunctionGenerated = source.generate(DATE_STRING_GENERATOR);
        Assert.assertNotNull(composedFunctionGenerated);
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
        final String ascii = source.getString(CodePoints.ASCII);

        final String alpha = source.getString(CodePoints.ALPHA);
        for (char c : alpha.toCharArray()) {
            Assert.assertTrue(Character.isAlphabetic(c));
        }

        final String alphaNum = source.getString(CodePoints.ALPHANUMERIC);
        for (char c : alpha.toCharArray()) {
            Assert.assertTrue(Character.isAlphabetic(c) || Character.isDigit(c));
        }
    }

    private enum AnEnum { A, B, C }

    @Test
    public void canGetAnEnum() throws Exception
    {
        AnEnum anEnum = source.getEnum(AnEnum.class);

        Assert.assertTrue(anEnum == AnEnum.A || anEnum == AnEnum.B || anEnum == AnEnum.C);
    }

    @Test
    public void canGetFromCollection() throws Exception
    {
        AnEnum anEnum = source.from(AnEnum.values());
        Assert.assertTrue(anEnum == AnEnum.A || anEnum == AnEnum.B || anEnum == AnEnum.C);

        Integer i = source.from(Arrays.asList(1, 2, 3));
        Assert.assertTrue(i <= 3);
        Assert.assertTrue(1 <= i);
    }

    @Test
    public void canGetAMap() throws Exception
    {
       final Map<String, String> m = source.getMap(
               s -> s.getString(CodePoints.ALPHANUMERIC),
               s -> s.getString(CodePoints.ALPHA));

       m.keySet().forEach(s -> {
           for (char c : s.toCharArray()) {
               Assert.assertTrue(Character.isAlphabetic(c) || Character.isDigit(c));
           }
       });

        m.values().forEach(s -> {
            for (char c : s.toCharArray()) {
                Assert.assertTrue(Character.isAlphabetic(c));
            }
        });
    }

    @Test
    public void canGetAMapWhereTheValueGenTakesKeyAsAnArg() throws Exception
    {
        final Map<String, String> m = source.getMap(
                s -> s.getString(CodePoints.ALPHANUMERIC),
                (s, k) -> k + s.getString(CodePoints.ALPHA));

        m.keySet().forEach(key -> {
            for (char c : key.toCharArray()) {
                Assert.assertTrue(Character.isAlphabetic(c) || Character.isDigit(c));
            }
            final String value = m.get(key);
            Assert.assertTrue(value.startsWith(key));
            for (char c : value.substring(key.length()).toCharArray()) {
                Assert.assertTrue(Character.isAlphabetic(c));
            }
        });
    }

    @Test
    public void canUseGeneratorsFromSource() throws Exception
    {
        GeneratorMapTestClass generated = source.generate(GeneratorMapTestClass.class);
        Assert.assertNotNull(generated);
    }

    @Test
    @Seed(1234567)
    @Trials(1)
    public void annotationsWork() throws Exception
    {
        Assert.assertEquals(-9223372036854775808L, source.getLong());
    }

    @Test
    public void reflectiveOverPrimitives() throws Exception
    {
        final Long aLong = source.reflectively(Long.class);
        final Long anotherLong = source.reflectively(long.class);
    }

    @Test
    public void reflectiveApi() throws Exception
    {
        final Date aDate = source.reflectively(Date.class.getConstructor(long.class));
        Assert.assertNotNull(aDate);

        final ClassWithStaticConstructor aClass =
                source.generate(s -> ClassWithStaticConstructor.constructor(s.reflectively(ClassWithConstructor.class)));
        Assert.assertNotNull(aClass);
    }

    public static <T, V> Supplier<V> bind(Function<T, V> f, T input)
    {
        return () -> f.apply(input);
    }

    public static Date generateDate(Source s)
    {
        s.pushInterval();
        final Date generatedValue = Date.from(Instant.ofEpochSecond(s.getInt()));
        s.popInterval(generatedValue);

        return generatedValue;
    }

    public static class GeneratorMapTestClass {
        public final String s;

        public GeneratorMapTestClass(String s)
        {
            this.s = s;
        }
    }

    public static class ClassWithConstructor
    {
        public ClassWithConstructor(GeneratorMapTestClass c)
        {
        }
    }

    public static class ClassWithStaticConstructor
    {
        public static ClassWithStaticConstructor constructor(ClassWithConstructor c)
        {
            return new ClassWithStaticConstructor();
        }
    }
}
