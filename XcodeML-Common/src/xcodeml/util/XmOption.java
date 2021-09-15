package xcodeml.util;

import java.util.ArrayList;
import xcodeml.util.XmLanguage;

/**
 * Decompile option.
// * @deprecated Not thread-safe.
 */
// @Deprecated  // avoid warning
public class XmOption
{
    public static final int COMP_VENDOR_GNU = 'G';
    public static final int COMP_VENDOR_INTEL = 'I';

    /** if suppress to write line directives */
    private static boolean _suppressLineDirective = false;

    /** if compiling Xcalable MP is enabled */
    private static boolean _xcalableMP = false;
    private static boolean _xcalableMPthreads = false;
    private static boolean _xcalableMPGPU = false;
    private static boolean _xcalableMPasync = false;

    private static boolean _tlog = false;
    private static boolean _Fonesided = false;

    /** if compiling XcalableACC is enabled */
    private static boolean _xcalableACC = false;

    /** if compiling OpenMP is enabled */
    private static boolean _openMP = false;

    /** if compiling OpenMP only target is enabled */
    private static boolean _openMPonlyTarget = false;

    /** if compiling coarray is enabled */
    private static boolean _coarray = false;

    /** if debug output is enabled */
    private static boolean _debugOutput = false;

    /** ensure left to right evaluation of mathematical expression **/
    private static boolean _addPar = false;

    /** target language ID */
    private static XmLanguage _language = XmLanguage.C;

    /** Name of tht main function */
    private static String _mainName = "";

    /** if transforming Fortran IO statement as atomic operation */
    private static boolean _isAtomicIO = false;

    /** backend compiler vendor */
    private static int _compilerVendor = COMP_VENDOR_GNU;

    /** if supressing generation of USE statement for coarray runtime
     *  (TEMPORARY)
     */
    private static boolean _coarrayUseStatement = false;
    private static ArrayList<String> _coarrayEntryNames = new ArrayList<>();

    private static int _pointer_scalar_size = 8;  // This value for gcc-8.3.0
    private static int _pointer_array_size  = 40; // This value for gcc-8.3.0
    private static int _pointer_diff_size   = 24; // This value for gcc-8.3.0
  
    private XmOption()
    {
    }

    /**
     * Sets compiler to or not to suppress to write line directives.
     *
     * @param enable true then compiler suppress to write line directives.
     */
    public static void setIsSuppressLineDirective(boolean enable)
    {
        _suppressLineDirective = enable;
    }

    /**
     * Checks does decompiler suppress line directives.
     *
     * @return true if compiler suppress to write line directives.
     */
    public static boolean isSuppressLineDirective()
    {
        return _suppressLineDirective;
    }

    /**
     * Sets compiler to or not to translate XcalableMP directive.
     *
     * @param enable true then translate XcalableMP directive.
     */
    public static void setIsXcalableMP(boolean enable)
    {
        _xcalableMP = enable;
    }

    /**
     * Checks does compiler translate XcalableMP directive.
     *
     * @return true if compiler translate XcalableMP directive.
     */
    public static boolean isXcalableMP()
    {
        return _xcalableMP;
    }

    /**
     * Sets compiler to or not to translate XcalableMP-threads directive.
     */
    public static void setIsXcalableMPthreads(boolean enable)
    {
        _xcalableMPthreads = enable;
    }

    /**
     * Checks does compiler translate XcalableMP-threads directive.
     */
    public static boolean isXcalableMPthreads()
    {
        return _xcalableMPthreads;
    }

    /**
     * Sets compiler to or not to translate XcalableMP-GPU directive.
     */
    public static void setIsXcalableMPGPU(boolean enable)
    {
        _xcalableMPGPU = enable;
    }

    /**
     * Checks does compiler translate XcalableMP-GPU directive.
     */
    public static boolean isXcalableMPGPU()
    {
        return _xcalableMPGPU;
    }

    /**
     * Sets whether the compiler supports asynchronous communications or not.
     */
    public static void setIsAsync(boolean enable)
    {
        _xcalableMPasync = enable;
    }

    /**
     * Checks if the compiler supports asynchronous communications.
     */
    public static boolean isAsync()
    {
        return _xcalableMPasync;
    }

    public static void setIsXcalableACC(boolean enable)
    {
        _xcalableACC = enable;
    }

    /**
     * Checks does compiler translate XcalableMP directive.
     *
     * @return true if compiler translate XcalableMP directive.
     */
    public static boolean isXcalableACC()
    {
        return _xcalableACC;
    }

    /**
     * Sets compiler to or not to use tlog for MPI.
     */
    public static void setTlogMPIisEnable(boolean enable)
    {
        _tlog = enable;
    }

    /**
     * Sets compiler to or not to use one-sided functions in Fortran.
    */
    public static void setFonesided(boolean enable)
    {
        _Fonesided = enable;
    }

    public static boolean isFonesided()
    {
        return _Fonesided;
    }

    /**
     * Checks does compiler use tlog for MPI.
     */
    public static boolean tlogMPIisEnable()
    {
        return _tlog;
    }

    /**
     * Sets compiler to or not to translate OpenMP directive.
     *
     * @param enable true then translate OpenMP directive.
     */
    public static void setIsOpenMP(boolean enable)
    {
        _openMP = enable;
    }

    /**
     * Sets compiler to or not to translate OpenMP only target directive.
     *
     * @param enable true then translate OpenMP only target directive.
     */
    public static void setIsOpenMPonlyTarget(boolean enable)
    {
     	_openMPonlyTarget = enable;
    }

    /**
     * Checks does compiler translate OpenMP directive.
     *
     * @return true if compiler translate OpenMP directive.
     */
    public static boolean isOpenMP()
    {
        return _openMP;
    }

    /**
     * Checks does compiler translate OpenMP only target directive.
     *
     * @return true if compiler translate OpenMP only target directive.
     */
    public static boolean isOpenMPOnlyTarget()
    {
        return _openMPonlyTarget;
    }

    /**
     * Sets compiler to or not to translate coarrays
     *
     * @param enable true then translate coarrays.
     */
    public static void setIsCoarray(boolean enable)
    {
        _coarray = enable;
    }

    /**
     * Checks does compiler translate coarrays.
     *
     * @return true if compiler translate coarrays.
     */
    public static boolean isCoarray()
    {
        return _coarray;
    }

    /**
     * Adds a name of coarray runtime library
     *
     * @param name added to the list of coarray runtime library
     */
    public static void addToCoarrayEntryNames(String name)
    {
        _coarrayEntryNames.add(name);
    }

    /**
     * Gets the list of coarray runtime library
     *
     * @return the list of coarray runtime library
     */
    public static ArrayList<String> getCoarrayEntryNames()
    {
        return _coarrayEntryNames;
    }

    /**
     * Return true if debug output enabled.
     */
    public static boolean isDebugOutput()
    {
        return _debugOutput;
    }

    /**
     * Set debug output.
     */
    public static void setDebugOutput(boolean enable)
    {
        _debugOutput = enable;
    }

    /**
     * Set language
     */
    public static void setLanguage(XmLanguage lang)
    {
        _language = lang;
    }

    /**
     * Get language
     */
    public static XmLanguage getLanguage()
    {
        return _language;
    }

    /**
     * Set name of the main function
     */
    public static void setMainName(String main_name)
    {
        _mainName = main_name;
    }

    /**
     * Get name of the main function
     */
    public static String getMainName()
    {
        return _mainName;
    }

    /**
     * Return if the language is C
     */
    public static boolean isLanguageC()
    {
        return _language.equals(XmLanguage.C);
    }

    /**
     * Return if the language is Fortran
     */
    public static boolean isLanguageF()
    {
        return _language.equals(XmLanguage.F);
    }

    /**
     * Return compiler vendor constant. (COMP_VENDOR_*)
     */
    public static int getCompilerVendor()
    {
        return _compilerVendor;
    }

    /**
     * Set compiler vendor constant. (COMP_VENDOR_*)
     */
    public static void setCompilerVendor(int vendor)
    {
        _compilerVendor = vendor;
    }

    /**
     * Get if or not IO statements are transformed to atomic operation.
     */
    public static boolean isAtomicIO()
    {
        return _isAtomicIO || _compilerVendor == COMP_VENDOR_INTEL;
    }

    /**
     * Set if or not IO statements are transformed to atomic operation.
     */
    public static void setIsAtomicIO(boolean atomicIO)
    {
        _isAtomicIO = atomicIO;
    }

    /**
     * Set/get suboption -fcoarray-use-statement (boolean)
     */
    public static void setCoarrayUseStatement(boolean coarrayUseStatement)
    {
        _coarrayUseStatement = coarrayUseStatement;
    }

    public static boolean coarrayUseStatement()
    {
        return _coarrayUseStatement;
    }

    /**
     * Sets compiler to ensure left to right evaluation of mathematical
     * expression.
     *
     * @param enable true then compiler add parenthesis to ensure evaluation.
     */
    public static void setAddPar(boolean enable)
    {
        _addPar = enable;
    }

    /**
     * Checks if the add parenthesis options is enabled.
     *
     * @return true if add parenthesis option is enabled.
     */
    public static boolean isAddParEnabled()
    {
        return _addPar;
    }

    public static void setPointerScalarSize(int value)
    {
	    _pointer_scalar_size = value;
    }

    public static int getPointerScalarSize()
    {
	    return _pointer_scalar_size;
    }
  
    public static void setPointerArraySize(int value)
    {
  	    _pointer_array_size = value;
    }

    public static int getPointerArraySize()
    {
  	    return _pointer_array_size;
	}
  
    public static void setPointerDiffSize(int value)
    {
	    _pointer_diff_size = value;
    }

    public static int getPointerDiffSize()
    {
     	return _pointer_diff_size;
    }
}
