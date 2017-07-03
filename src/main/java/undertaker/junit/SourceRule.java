package undertaker.junit;

import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;
import undertaker.Source;
import undertaker.generators.IntGen;

public class SourceRule implements TestRule, IntGen {

    private Source source;

    @Override
    public Statement apply(Statement statement, Description description) {
        return statement;
    }

    public int getInt(int min, int max)
    {
        return IntGen.moveIntoRange(IntGen.getRawInt(source), min, max);
    }
}
