package lib;

public class Module {

    private final String moduleName;
    private final ModuleCode moduleCode;
    private int moduleMarks;

    // Enum used to store the module code
    public enum ModuleCode {
        CI101, CI405, CI535
    }

    /**
     * Module constructor
     *
     * @param moduleNameInput The name of the module
     * @param moduleCodeInput The code of the module
     */
    public Module(String moduleNameInput, ModuleCode moduleCodeInput) {
        moduleName = moduleNameInput;
        moduleCode = moduleCodeInput;
        moduleMarks = 0;
    }

/////////// RETURNING ///////////

    /**
     * Used to return the module name
     *
     * @return The module name
     */
    public String returnModuleName() {
        return moduleName;
    }

    /**
     * Used to return the module code
     *
     * @return The module code
     */
    public ModuleCode returnModuleCode() {
        return moduleCode;
    }

    /**
     * Used to return the module marks
     *
     * @return The module marks
     */
    public int returnModuleMarks() {
        return moduleMarks;
    }

/////////// UPDATING ///////////

    /**
     * Used to update the module marks
     *
     * @param marksToAdd The marks to add to the module
     */
    public void updateModuleMarks(int marksToAdd) {
        moduleMarks = moduleMarks + marksToAdd;
    }
}