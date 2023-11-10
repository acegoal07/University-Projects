package lib;
import java.util.Arrays;
import java.util.stream.Stream;

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
   
   /**
    * Student constructor
    * @param fNameInput The first name of the student
    * @param sNameInput The surname of the student
    * @param ageInput The age of the student
    * @param birthDayInput The day the student was born
    * @param birthMonthInput The month the student was born
    * @param birthYearInput The year the student was born
    * @param idNumberInput The ID number of the student
    * @param courseInput The course the student is on
    * @param modulesInput The modules the student is on
    */
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
   
/////////// RETURNING ///////////

   /**
    * Used to return the students name
    * @return The students name
    */
   public String returnName() {
      return fName+" "+sName;
   }
   /**
    * Used to return the students details
    * @return The students last name
    */
   public String returnLastName() {
      return sName;
   }
   /**
    * Used to return the students details
    * @return The students first name
    */
   public String returnFirstName() {
      return fName;
   }
   /**
    * Used to return the students age
    * @return The students age
    */
   public Integer returnAge() {
      return age;
   }
   /**
    * Used to return the students birthday
    * @return The students birthday
    */
   public String returnBirthday() {
      return birthDay+"/"+birthMonth+"/"+birthYear;
   }
   /**
    * Used to return the students ID number
    * @return The students ID number
    */
   public Integer returnIdNumber() {
      return idNumber;
   }
   /**
    * Used to return the students course
    * @return The students course
    */
   public String returnCourse() {
      return course;
   }
   /**
    * Used to return the students modules
    * @return The students modules
    */
   public String[] returnModulesNames() {
      return Arrays.stream(modules).map(Module::returnModuleName).toArray(String[]::new);
   }
   /**
    * Used to return the students modules
    * @return The students modules
    */
   public String[] returnModulesCodes() {
      return Arrays.stream(modules).map(Module::returnModuleCode).toArray(String[]::new);
   }
   /**
    * Used to return the students details
    * @return The students details
    */
   public String returnDetails() {
      return "Name: "+returnName()+"\nAge: "+returnAge()+"\nBirthday: "+returnBirthday()+"\nID Number: "+returnIdNumber()+"\nCourse: "+returnCourse()+"\nModules: "+String.join(",", returnModulesNames());
   }   

/////////// ADDING AND REMOVING ///////////

   /**
    * Used to add modules to the student
    * @param modulesInput The modules you want to add
    */
   public void addModule(Module ... modulesInput) {
      modules = Stream.concat(Arrays.stream(modules), Arrays.stream(modulesInput)).toArray(Module[]::new);
   }
   /**
    * Used to remove a module from the student
    * @param moduleCodeInput The module code of the module you want to remove
    */
   public void removeModule(String moduleCodeInput) {
      modules = Arrays.stream(modules).filter(m -> !m.returnModuleCode().equals(moduleCodeInput)).toArray(Module[]::new);
   }
}