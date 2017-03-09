# NDK Mapping

A tool for make class mapping between JVM (use kotlin) and JNI.

It makes the development of JNI much convenient.

- - -

Usage:

**Generate JNI Code:**

```
ndkmapping <options> <Kotlin Class File Path>

    options:
        -l language (cpp, pas)
        -b build option (mk, mksh)
        -m max array size (must >= 0)
        -o output path
```

Sample:

```
ndkmapping -l cpp -b mksh -m 100 -a kotlin -o ./out/ ./classes/
```

**Generate Test Code:**

```
ndktester <options> <Kotlin Class File Path>

  options:
    -l language (java, kotlin)
    -x exported JNI code language (cpp, pas)
    -b build option (mk, mkshcp)
    -p base package name
    -c copy path
    -o output path
```

Sample:

```
ndktester -l kotlin -x cpp -b mkshcp -p com.sample.ndk -c ./jniLibs/ -o ./out/ ./classes/
```

You may use the generated code directly in real project or try the sample project([Click Download](http://www.rarnu.com/download/jniSample.zip)).

- - -

Build:

To build the NDKMapping and NDKTester, download [CodeTyphon](http://www.pilotlogic.com) and install it on your Linux or Mac.

You may open the ```lpr``` or ```lpi``` with CodeTyphon and compile it.
