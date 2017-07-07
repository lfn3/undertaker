package undertaker;

import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import undertaker.SourceRule;

public class SourceRuleTest {
    @Rule
    public SourceRule source = new SourceRule();

    @Test
    public void compilesAndRuns()
    {

    }

    @Test
    public void canGetAnInt()
    {
        int anInt = source.getInt();
        Assert.assertTrue(anInt >= Integer.MIN_VALUE);
        Assert.assertTrue(anInt <= Integer.MAX_VALUE);
    }


    @Test
    public void canGetAnBetweenMaxAndMin()
    {
        int anInt = source.getInt(0, 1);
        Assert.assertTrue(anInt == 0 || anInt == 1);
    }
}
