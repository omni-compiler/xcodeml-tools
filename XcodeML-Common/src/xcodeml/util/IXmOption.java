package xcodeml.util;

import java.util.ArrayList;

public interface IXmOption {
    int  COMP_VENDOR_GNU = 'G';
    int  COMP_VENDOR_INTEL = 'I';

    /**
     * Sets compiler to or not to suppress to write line directives.
     *
     * @param enable true then compiler suppress to write line directives.
     */
    void setIsSuppressLineDirective(boolean enable);

    /**
     * Checks does decompiler suppress line directives.
     *
     * @return true if compiler suppress to write line directives.
     */
    boolean isSuppressLineDirective();

    /**
     * Sets compiler to or not to translate XcalableMP directive.
     *
     * @param enable true then translate XcalableMP directive.
     */
    void setIsXcalableMP(boolean enable);

    /**
     * Checks does compiler translate XcalableMP directive.
     *
     * @return true if compiler translate XcalableMP directive.
     */
    boolean isXcalableMP();

    /**
     * Sets compiler to or not to translate XcalableMP-threads directive.
     */
    void setIsXcalableMPthreads(boolean enable);

    /**
     * Checks does compiler translate XcalableMP-threads directive.
     */
    boolean isXcalableMPthreads();

    /**
     * Sets compiler to or not to translate XcalableMP-GPU directive.
     */
    void setIsXcalableMPGPU(boolean enable);

    /**
     * Checks does compiler translate XcalableMP-GPU directive.
     */
    boolean isXcalableMPGPU();

    /**
     * Sets whether the compiler supports asynchronous communications or not.
     */
    void setIsAsync(boolean enable);

    /**
     * Checks if the compiler supports asynchronous communications.
     */
    boolean isAsync();

    void setIsXcalableACC(boolean enable);

    /**
     * Checks does compiler translate XcalableMP directive.
     *
     * @return true if compiler translate XcalableMP directive.
     */
    boolean isXcalableACC();

    /**
     * Sets compiler to or not to use tlog for MPI.
     */
    void setTlogMPIisEnable(boolean enable);

    /**
     * Sets compiler to or not to use one-sided functions in Fortran.
     */
    void setFonesided(boolean enable);

    boolean isFonesided();

    /**
     * Checks does compiler use tlog for MPI.
     */
    boolean tlogMPIisEnable();

    /**
     * Sets compiler to or not to translate OpenMP directive.
     *
     * @param enable true then translate OpenMP directive.
     */
    void setIsOpenMP(boolean enable);

    /**
     * Sets compiler to or not to translate OpenMP only target directive.
     *
     * @param enable true then translate OpenMP only target directive.
     */
    void setIsOpenMPonlyTarget(boolean enable);

    /**
     * Checks does compiler translate OpenMP directive.
     *
     * @return true if compiler translate OpenMP directive.
     */
    boolean isOpenMP();

    /**
     * Checks does compiler translate OpenMP only target directive.
     *
     * @return true if compiler translate OpenMP only target directive.
     */
    boolean isOpenMPOnlyTarget();

    /**
     * Sets compiler to or not to translate coarrays
     *
     * @param enable true then translate coarrays.
     */
    void setIsCoarray(boolean enable);

    /**
     * Checks does compiler translate coarrays.
     *
     * @return true if compiler translate coarrays.
     */
    boolean isCoarray();

    /**
     * Adds a name of coarray runtime library
     *
     * @param name added to the list of coarray runtime library
     */
    void addToCoarrayEntryNames(String name);

    /**
     * Gets the list of coarray runtime library
     *
     * @return the list of coarray runtime library
     */
    ArrayList<String> getCoarrayEntryNames();

    /**
     * Return true if debug output enabled.
     */
    boolean isDebugOutput();

    /**
     * Set debug output.
     */
    void setDebugOutput(boolean enable);

    /**
     * Set language
     */
    void setLanguage(XmLanguage lang);

    /**
     * Get language
     */
    XmLanguage getLanguage();

    /**
     * Set name of the main function
     */
    void setMainName(String main_name);

    /**
     * Get name of the main function
     */
    String getMainName();

    /**
     * Return if the language is C
     */
    boolean isLanguageC();

    /**
     * Return if the language is Fortran
     */
    boolean isLanguageF();

    /**
     * Return compiler vendor constant. (COMP_VENDOR_*)
     */
    int getCompilerVendor();

    /**
     * Set compiler vendor constant. (COMP_VENDOR_*)
     */
    void setCompilerVendor(int vendor);

    /**
     * Get if or not IO statements are transformed to atomic operation.
     */
    boolean isAtomicIO();

    /**
     * Set if or not IO statements are transformed to atomic operation.
     */
    void setIsAtomicIO(boolean atomicIO);

    /**
     * Set/get suboption -fcoarray-use-statement (boolean)
     */
    void setCoarrayUseStatement(boolean coarrayUseStatement);

    boolean coarrayUseStatement();

    /**
     * Sets compiler to ensure left to right evaluation of mathematical
     * expression.
     *
     * @param enable true then compiler add parenthesis to ensure evaluation.
     */
    void setAddPar(boolean enable);

    /**
     * Checks if the add parenthesis options is enabled.
     *
     * @return true if add parenthesis option is enabled.
     */
    boolean isAddParEnabled();

    void setPointerScalarSize(int value);

    int getPointerScalarSize();

    void setPointerArraySize(int value);

    int getPointerArraySize();

    void setPointerDiffSize(int value);

    int getPointerDiffSize();
}
