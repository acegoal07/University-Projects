public class Module {
   public String moduleName;
   public String moduleCode;

   public Module(String moduleNameInput, String moduleCodeInput) {
      moduleName = moduleNameInput;
      moduleCode = moduleCodeInput;
   }
   // Return module name
   public String returnModuleName() {
      return moduleName;
   }
   // Return module code
   public String returnModuleCode() {
      return moduleCode;
   }
}