package net.lfn3.undertaker;

public class UniqueInputValuesExhaustedException extends IllegalStateException {
    public UniqueInputValuesExhaustedException(String s) {
        super("Ran out of valid values to generate after applying hints.\n" +
                "This means that when generating a set or another collection that requires unique values,\n" +
                "there weren't any valid values remaining for the to produce.\n"
                + s + "\n" +
                "If your generator has a required minimum, you might want to decrease it.\n" +
                "If you can't figure out why this is happening, applying the Debug flag (see the readme) will let you look at\n" +
                "the generated intervals and ranges, and should give you a better idea of what's going on.");
    }
}
