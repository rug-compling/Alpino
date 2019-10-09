import net.sf.saxon.Query;

public class SaxonWrap extends Query
{
    public static void main (String argv[])
    {
        int i;
        String queryFile;
        String dataFile;
        String[] commandArray = new String[3]; // -s file.xml queryfile.xq

        SaxonWrap s;

        if (argv.length < 2) {
            System.err.println("Usage: saxon-wrap file.xq datafile [datafile] ...");
            System.exit(1);
        }

        s = new SaxonWrap();

        queryFile = argv[0];

        commandArray[0] = "-s";
        commandArray[2] = queryFile;

        for (i = 1; i < argv.length; i++) {
            commandArray[1] = argv[i];
            // System.out.println("Processing " + argv[i] + "...");
            s.doQuery(commandArray, "java net.sf.saxon.Query");
        }
    }
}



/*
Local Variables:
compile-command: "javac -classpath /users1/gosse/src/saxon8-8/saxon8.jar:. SaxonWrap.java"
End:
*/
