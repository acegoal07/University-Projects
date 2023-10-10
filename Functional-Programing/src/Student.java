import java.util.Arrays;

public class Student {
   private String fName;
   private String sName;
   private Integer age;
   private Integer birthDay;
   private Integer birthMonth;
   private Integer birthYear;
   private Integer idNumber;
   private String course;
   private Module[] modules;
   // Student constructor
   public Student(String fNameInput, String sNameInput, Integer ageInput, Integer birthDayInput, Integer birthMonthInput, Integer birthYearInput, Integer idNumberInput, String courseInput, Module[] modulesInput) {
      fName = fNameInput;
      sName = sNameInput;
      idNumber = idNumberInput;
      birthDay = birthDayInput;
      birthMonth = birthMonthInput;
      birthYear = birthYearInput;
      age = ageInput;
      course = courseInput;
      modules = Arrays.copyOf(modulesInput, modulesInput.length);
   }
   // Returns student name
   public String returnName() {
      return fName+" "+sName;
   }
   // Return age
   public Integer returnAge() {
      return age;
   }
   // Return birthday
   public String returnBirthday() {
      return birthDay+"/"+birthMonth+"/"+birthYear;
   }
   // Return id number
   public Integer returnIdNumber() {
      return idNumber;
   }
   // return course
   public String returnCourse() {
      return course;
   }
   // Add module
   public void addModule(Module ... modulesInput) {
      modules = modulesInput;
   }
   // Remove modules using streams
   public void removeModule(String moduleCodeInput) {
      modules = Arrays.stream(modules).filter(m -> !m.returnModuleCode().equals(moduleCodeInput)).toArray(Module[]::new);
   }
   // Returns each modules name without joining them
   public String[] returnModulesNames() {
      return Arrays.stream(modules).map(Module::returnModuleName).toArray(String[]::new);
   }
   // Returns each modules code without joining them
   public String[] returnModulesCodes() {
      return Arrays.stream(modules).map(Module::returnModuleCode).toArray(String[]::new);
   }
   // Return student details
   public String returnDetails() {
      return "Name: "+returnName()+"\nAge: "+returnAge()+"\nBirthday: "+returnBirthday()+"\nID Number: "+returnIdNumber()+"\nCourse: "+returnCourse()+"\nModules: "+String.join(",", returnModulesNames());
   }
}