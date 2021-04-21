/*
 * @author Mikhail Zhigun
 */
package xcodeml.f.util;

import java.nio.file.Path;
import java.nio.file.Paths;

public class FxCompiler
{
    static native int execute(CLIOptions opts) throws Exception;

    public static void main(String[] args) throws Exception
    {
        final Path WORKING_DIR = Paths.get(System.getProperty("user.dir"));
        CLIOptions opts = CLIOptions.parseCmdlineArguments(args, WORKING_DIR);
        if (opts != null)
        {
            System.loadLibrary("ffront-jni");
            System.exit(execute(opts));
        } else
        {
            System.exit(0);
        }
    }
}
