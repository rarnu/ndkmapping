#include <jni.h>

class JNIUtils {
public:
    static jintArray arrayToIntArray(JNIEnv *env, int arr[]);
    static jdoubleArray arrayToDoubleArray(JNIEnv *env, double arr[]);
    static jbooleanArray arrayToBooleanArray(JNIEnv *env, bool arr[]);
    static jbyteArray arrayToByteArray(JNIEnv *env, unsigned char arr[]);
    static jcharArray arrayToCharArray(JNIEnv *env, char arr[]);
    static jshortArray arrayToShortArray(JNIEnv *env, short arr[]);
    static jlongArray arrayToLongArray(JNIEnv *env, long long arr[]);
    static jfloatArray arrayToFloatArray(JNIEnv *env, float arr[]);
    static jobjectArray arrayToStringArray(JNIEnv *env, string arr[]);
    static jobjectArray arrayToObjectArray(JNIEnv *env, void* arr[]);
};
