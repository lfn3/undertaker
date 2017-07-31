package undertaker.junit;

import org.junit.rules.TestRule;
import undertaker.junit.generators.*;

public interface Source extends BoolGen, ByteGen, IntGen, ListGen, LongGen, TestRule
{
    long pushInterval(String intervalName);
    void popInterval(long intervalId, Object generatedValue);
}
