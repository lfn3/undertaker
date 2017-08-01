package undertaker.junit.junit;

import org.junit.Ignore;
import org.junit.Test;
import undertaker.junit.Source;
import undertaker.junit.SourceRule;

public class NoSourceRuleTest {
    @Test
    @Ignore
    public void showsErrorMessageIfSourceRuleIsMissing() throws Exception {
        Source source = new SourceRule();
        source.getBool();
    }
}
