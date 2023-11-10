package lib;
public class Module {

   private String moduleName;
   private String moduleCode;
   
   /**
    * Module constructor
    * @param moduleNameInput The name of the module
    * @param moduleCodeInput The code of the module
    */
   public Module(String moduleNameInput, String moduleCodeInput) {
      moduleName = moduleNameInput;
      moduleCode = moduleCodeInput;
   }

/////////// RETURNING ///////////

   /**
    * Used to return the module name
    * @return The module name
    */
   public String returnModuleName() {
      return moduleName;
   }
   /**
    * Used to return the module code
    * @return The module code
    */
   public String returnModuleCode() {
      return moduleCode;
   }
}