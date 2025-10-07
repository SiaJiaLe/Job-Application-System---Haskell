{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Monad law, left identity" #-}
module Main where
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import System.IO (hFlush, stdout)
import System.Process ( system )
import Data.IORef
import Control.Exception (IOException, catch )
import Data.List (find)
import Control.Concurrent (threadDelay)
import Text.Regex.TDFA ((=~))
import System.Console.ANSI
import Control.Monad.State

-- Recruiter data type
data Recruiter = Recruiter
    { recruiterName :: String
    , recruiterEmail :: String 
    , recruiterPassword :: String 
    , recruiterContact :: Int 
    , companyName :: String 
    , jobTitle :: String 
    , positionOffered :: String
    } deriving (Show, Eq)

-- Applicant data type
data Applicant = Applicant
    { applicantName :: String
    , applicantEmail :: String 
    , applicantPassword :: String 
    , applicantContact :: Int 
    , levelOfEducation :: String 
    , courseStudied :: String 
    , applicantExperience :: String
    } deriving (Show, Eq)

-- Job data type
data Job = Job
    { company :: String 
    , job :: String 
    , position :: String 
    , educationalLevel :: String 
    , experience :: String 
    , relatedFields :: String 
    , shift :: String 
    , salary :: String
    } deriving (Show, Eq)

-- Application data type
data Application = Application
    { applicantDetails :: Applicant 
    , jobDetails :: Job
    } deriving (Show, Eq)

-- Represents a user, which can either be a recruiter or an applicant.
data User = RecruiterUser Recruiter | ApplicantUser Applicant deriving (Show, Eq)

-- IORef type aliases for mutable state of recruiters and applicants lists.
type RecruiterList = IORef [Recruiter]
type ApplicantList = IORef [Applicant]

-- Initializes an empty recruiter list as an IORef.
initRecruiterList :: IO RecruiterList
initRecruiterList = newIORef []

-- Initializes an empty applicant list as an IORef.
initApplicantList :: IO ApplicantList
initApplicantList = newIORef []

-- Type alias for a list of jobs.
type JobList = [Job]

-- Type class defining common operations for users (recruiters and applicants).
class Users a where
    getName :: a -> String
    getEmail :: a -> String
    getPassword :: a -> String
    getContact :: a -> Int
    getLevelOfEducation :: a -> String
    getCourseStudied :: a -> String
    getUserExperience :: a -> String
    getCompanyName :: a -> String
    getJobTitle :: a -> String
    getJobPosition :: a -> String
    setName :: String -> a -> a
    setEmail :: String -> a -> a
    setPassword :: String -> a -> a
    setContact :: Int -> a -> a
    setLevelOfEducation :: String -> a -> a
    setCourseStudied :: String -> a -> a
    setExperience :: String -> a -> a
    setCompanyName :: String -> a -> a
    setJobTitle :: String -> a -> a
    setPosition :: String -> a -> a
    showProfile :: a -> String

-- Implementation of the Users type class for Recruiter.
instance Users Recruiter where
    -- getter
    getName = recruiterName
    getEmail = recruiterEmail
    getPassword = recruiterPassword
    getContact = recruiterContact
    getCompanyName = companyName
    getJobTitle = jobTitle
    getJobPosition = positionOffered
    -- setter
    setName newName recruiter = recruiter {recruiterName = newName}
    setEmail newEmail recruiter = recruiter {recruiterEmail = newEmail}
    setPassword newPassword recruiter = recruiter {recruiterPassword = newPassword}
    setContact newContact recruiter = recruiter {recruiterContact = newContact}
    setCompanyName newCompanyName recruiter = recruiter {companyName = newCompanyName}
    setJobTitle newJobTitle recruiter = recruiter {jobTitle = newJobTitle}
    setPosition newPosition recruiter = recruiter {positionOffered = newPosition}
    showProfile recruiter =
        "Name: " <> getName recruiter <>
        "\nEmail: " <> getEmail recruiter <>
        "\nPassword: " <> getPassword recruiter <>
        "\nContact: " <> show (getContact recruiter) <>
        "\nCompany Name: " <> getCompanyName recruiter <>
        "\nJob Title: " <> getJobTitle recruiter <>
        "\nPosition Offered: " <> getJobPosition recruiter 

-- Implementation of the Users type class for Applicant.
instance Users Applicant where
    -- Getters
    getName = applicantName
    getEmail = applicantEmail
    getPassword = applicantPassword
    getContact = applicantContact
    getLevelOfEducation = levelOfEducation
    getCourseStudied = courseStudied
    getUserExperience = applicantExperience
    -- Setters
    setName newName applicant = applicant {applicantName = newName}
    setEmail newEmail applicant = applicant {applicantEmail = newEmail}
    setPassword newPassword applicant = applicant {applicantPassword = newPassword}
    setContact newContact applicant = applicant {applicantContact = newContact}
    setLevelOfEducation newLevelOfEducation applicant = applicant {levelOfEducation = newLevelOfEducation}
    setCourseStudied newCourseStudied applicant = applicant {courseStudied = newCourseStudied}
    setExperience newExperience applicant = applicant {applicantExperience = newExperience}
    showProfile applicant =
        "Name: " <> getName applicant <>
        "\nEmail: " <> getEmail applicant <>
        "\nContact: " <> show (getContact applicant) <>
        "\nLevel of Education: " <> getLevelOfEducation applicant <>
        "\nCourse studied: " <> getCourseStudied applicant <>
        "\nExperience: " <> getUserExperience applicant

-- Type class for defining operations on job vacancies.
class JobVacancy a where
    getCompany :: a -> String
    getJob :: a -> String
    getPosition :: a -> String
    getEducationalLevel :: a -> String
    getExperience :: a -> String
    getRelatedFields :: a -> String
    getShift :: a -> String
    getSalary :: a -> String
    showVacancy :: a -> String
    showVacancy jobVacancy =
        "Company Name: " <> getCompany jobVacancy <>
        "\nJob Scope: " <> getJob jobVacancy <>
        "\nPosition: " <> getPosition jobVacancy <>
        "\nLevel of Education: " <> getEducationalLevel jobVacancy <>
        "\nExperience: " <> getExperience jobVacancy <>
        "\nRelated Fields: " <> getRelatedFields jobVacancy <>
        "\nShift: " <> getShift jobVacancy <>
        "\nSalary: " <> getSalary jobVacancy

-- Implementation of the JobVacancy type class for Job.
instance JobVacancy Job where
    getCompany = company
    getJob = job
    getPosition = position
    getEducationalLevel = educationalLevel
    getExperience = experience
    getRelatedFields = relatedFields
    getShift = shift
    getSalary = salary

instance ToRecord Recruiter where
    toRecord (Recruiter name email password contact company jobTitle position) =
        record [toField name, toField email, toField password, toField contact, toField company, toField jobTitle, toField position]

instance ToRecord Applicant where
    toRecord (Applicant name email password contact education course experience) =
        record [toField name, toField email, toField password, toField contact, toField education, toField course, toField experience]

instance ToRecord User where
    toRecord (RecruiterUser r) = toRecord r V.++ record ["Recruiter"]
    toRecord (ApplicantUser a) = toRecord a V.++ record ["Applicant"]

instance ToRecord Job where
    toRecord (Job company job position educationalLevel experience relatedFields shift salary) =
        record [toField company, toField job, toField position, toField educationalLevel, toField experience, toField relatedFields, toField shift, toField salary]

instance ToRecord Application where
    toRecord (Application applicantDetails jobDetails) =
        toRecord applicantDetails `mappend` toRecord jobDetails

instance FromRecord Recruiter where
    parseRecord v
        | V.length v == 7 = Recruiter <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> (read <$> v .! 3) <*> v .! 4 <*> v .! 5 <*> v .! 6
        | otherwise = fail "Invalid Recruiter record"

instance FromRecord Applicant where
    parseRecord v
        | V.length v == 7 = Applicant <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> (read <$> v .! 3) <*> v .! 4 <*> v .! 5 <*> v .! 6
        | otherwise = fail "Invalid Applicant record"

instance FromRecord User where
    parseRecord v
        | V.length v == 8 = do
            userType :: String <- v .! 7
            case userType of
                "Recruiter" -> RecruiterUser <$> parseRecord (V.init v)
                "Applicant" -> ApplicantUser <$> parseRecord (V.init v)
                _           -> fail "Unknown user type"
        | otherwise = fail "Invalid User record"

instance FromRecord Job where
    parseRecord v
        | V.length v == 8 = Job <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*> v .! 5 <*> v .! 6 <*> v .! 7
        | otherwise = fail "Invalid Job Record"

instance FromRecord Application where
    parseRecord v
        | V.length v == 15 = Application <$> parseRecord (V.take 7 v) <*> parseRecord (V.drop 7 v)
        | otherwise = fail "Invalid Application record"

-- Clears the console screen (Windows-specific).
clearscreen :: IO ()
clearscreen = 
    system "cls" >> return ()

-- Reads a CSV file and parses it into a list of records.
readCSV :: FromRecord a => FilePath -> IO [a]
readCSV filepath = 
    putStrLn ("Reading file: " ++ filepath) >> threadDelay 500000 >> 
    BL.readFile filepath `catch` handledReadError >>= \content ->
        case decode NoHeader content of
            Left err -> 
                putStrLn ("Error decoding file: " ++ filepath ++ "-" ++ err) >>return [] -- Return empty list if decoding fails
            Right vec -> 
                return (putStrLn ("Successfully read file: " ++ filepath)) >> return $ V.toList vec --Convert vector to list
    where
        handledReadError :: IOException -> IO BL.ByteString
        handledReadError err = 
            putStrLn ("Error: Unable to read file: " ++ filepath ++ ". Reason: " ++ show err) >> return BL.empty

-- Writes a list of records to a CSV file.
writetoCSV :: ToRecord a => FilePath -> [a] -> IO () 
writetoCSV filepath updatedList = 
    BL.writeFile filepath (encode updatedList)

-- Appends new data to an existing list of records.
appendNewData :: FromRecord a => [a] -> [a] -> [a]
appendNewData previouslist newData = previouslist `mappend` newData

-- Validates an email address using a regular expression.
validateEmail :: String -> Bool
validateEmail email = email =~ ("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" :: String)

-- Validates a phone number (10-11 digits) using a regular expression.
validatePhoneNumber :: String -> Bool
validatePhoneNumber phoneNum = phoneNum =~ ("^[0-9]{10,11}$" :: String)

-- Prompts the user to input a valid email address.
promptEmail :: IO String
promptEmail =
    putStrLn "Email: " >> hFlush stdout >> getLine >>= \email ->
        if validateEmail email
            then return email
        else
            putStrLn "Invalid email. Please try again..." >> threadDelay 1500000 >> promptEmail

-- Prompts the user to input a valid contact number.
promptContactNum :: IO String
promptContactNum = putStrLn "Contact (10/11 digits): " >> hFlush stdout >> getLine >>= \contactStr ->
    if validatePhoneNumber contactStr
        then return contactStr
    else
        putStrLn "Invalid phone number. Please try again..." >> threadDelay 1500000 >> promptContactNum

-- Checks if an email is already taken in the user list.
isEmailTaken :: String -> [User] -> Bool
isEmailTaken email userList =
    any (\user -> case user of
        RecruiterUser recruiter -> recruiterEmail recruiter == email
        ApplicantUser applicant -> applicantEmail applicant == email) userList

-- State type for managing login-related computations.
-- State type: (ExpectedRole, [User])
type LoginState = State (String, [User])

-- Authenticates a user by email and password.
authenticateUser :: String -> String -> LoginState (Maybe User)
authenticateUser email password =
    get >>= \(expectedRole, userDataList) ->
    return (find (\user -> case user of
                    RecruiterUser recruiter ->
                        expectedRole == "Recruiter" &&
                        recruiterEmail recruiter == email &&
                        recruiterPassword recruiter == password
                    ApplicantUser applicant ->
                        expectedRole == "Applicant" &&
                        applicantEmail applicant == email &&
                        applicantPassword applicant == password) userDataList)

-- Handles user login for recruiters and applicants.
loginUser :: String -> RecruiterList -> ApplicantList -> [User] -> [Job] -> [Application] -> IO ()
loginUser expectedRole recruiterList applicantList userDataList jobList applications = 
    putStr "Enter your " >>
    promptEmail >>= \email ->

    putStrLn "Enter your password: " >>
    hFlush stdout >>
    getLine >>= \password ->

    pure (expectedRole, userDataList) >>= \initialState ->
        pure (evalState (authenticateUser email password) initialState) >>= \maybeUser ->
            case maybeUser of
                -- General role mismatch handler
                Just (RecruiterUser recruiter) ->
                        print ("Welcome Recruiter, " ++ recruiterName recruiter ++ "!") >> 
                        threadDelay 1000000 >> clearscreen >> showRecruiterMenu userDataList jobList recruiter applications

                Just (ApplicantUser applicant) ->
                        print ("Welcome Applicant, " ++ applicantName applicant ++ "!") >> 
                        threadDelay 1000000 >> clearscreen >> showApplicantMenu userDataList jobList applications applicant

                -- Handle invalid credentials
                Nothing -> putStrLn "Invalid email and password or invalid role. Please try again." >> 
                        threadDelay 1500000 >> clearscreen >> mainmenu recruiterList applicantList userDataList jobList applications

-- Converts a contact number from String to Int.
readContactStr :: String -> Int
readContactStr = read
-- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--                                                                  Functions for Recruiters
-- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Handles recruiter login or sign-up.
-- Displays a menu for the recruiter to choose between login and sign-up.
-- Based on the user's choice, redirects to the appropriate workflow.
recruiterLoginOrSignup :: String -> RecruiterList -> ApplicantList -> [User] -> [Job] -> [Application] -> IO ()
recruiterLoginOrSignup expectedRole recruiterList applicantList userDataList jobDataList applications =
    clearscreen >>
    putStrLn "Do you want to login or sign up?" >>
    putStrLn "1. Login" >>
    putStrLn "2. Sign Up" >>
    putStrLn "Choose an option (1/2):" >> hFlush stdout >>
    getLine >>= \choice ->
        case choice of
            "1" -> 
                clearscreen >>
                loginUser expectedRole recruiterList applicantList userDataList jobDataList applications
            "2" -> 
                clearscreen >>
                showRecruiterSignUpMenu recruiterList userDataList jobDataList applications
            _ -> putStrLn "Invalid choice. Please try again..." >> threadDelay 1500000 >> recruiterLoginOrSignup expectedRole recruiterList applicantList userDataList jobDataList applications

-- Handles the recruiter sign-up process.
-- Collects information such as name, email, password, contact number, company, job title, and position.
-- Validates the email to ensure it's not already taken, then creates a new recruiter and saves it.
showRecruiterSignUpMenu :: RecruiterList -> [User] -> [Job] -> [Application] -> IO ()
showRecruiterSignUpMenu recruiterList userDataList jobDataList applications =
    putStrLn "1. Name: " >> hFlush stdout >>
    getLine >>= \name ->
    putStr "2. " >> promptEmail >>= \email -> 
        if isEmailTaken email userDataList 
            then putStrLn "The email already exist. Please try another email." >> threadDelay 1500000 >> clearscreen >> showRecruiterSignUpMenu recruiterList userDataList jobDataList applications
        else
            putStrLn "3. Password: " >> hFlush stdout >>
            getLine >>= \password ->
            putStr "4. " >> promptContactNum >>= \contactStr -> 
            putStrLn "5. Company Name: " >> hFlush stdout >>
            getLine >>= \company ->
            putStrLn "6. Job Title: " >> hFlush stdout >>
            getLine >>= \job ->
            putStrLn "7. Position: " >> hFlush stdout >>
            getLine >>= \position ->
            -- Parse contact to Int
            pure (readContactStr contactStr) >>= \contact ->
            pure (Recruiter name email password contact company job position) >>= \newRecruiter -> 
            pure (appendNewData userDataList [RecruiterUser newRecruiter]) >>= \updatedUserList -> 
            writetoCSV "app/Users.csv" updatedUserList >>
            putStrLn "Sign Up Successful!" >> threadDelay 1500000 >>
            putStrLn ("Welcome " ++ name ++ "!") >> showRecruiterMenu updatedUserList jobDataList newRecruiter applications

-- Displays the main menu for recruiters.
-- Offers options to propose vacancies, check applications, view profile, edit profile, or exit.
showRecruiterMenu :: [User] -> [Job] -> Recruiter -> [Application] -> IO ()
showRecruiterMenu userDataList jobDataList recruiter applications =
    clearscreen >>
    putStrLn "1. Propose Vacancy" >>
    putStrLn "2. Check Application" >>
    putStrLn "3. Check Profile" >>
    putStrLn "4. Edit Profile" >>
    putStrLn "5. Exit" >>
    putStrLn "Choose an option (1-5): " >>
    hFlush stdout >> getLine >>= \choice ->
        case choice of
            "1" -> clearscreen >> proposeVacancy userDataList jobDataList recruiter applications
            "2" -> clearscreen >> checkApplicantJobApplication userDataList jobDataList recruiter applications
            "3" -> clearscreen >> checkRecruiterProfile userDataList jobDataList recruiter applications
            "4" -> clearscreen >> recruiterEditProfile userDataList jobDataList applications recruiter
            "5" -> writetoCSV "app/Users.csv" userDataList
            _ -> putStrLn "Invalid choice. Please try again..." >> threadDelay 1500000 >> showRecruiterMenu userDataList jobDataList recruiter applications

-- Allows a recruiter to propose a new job vacancy.
-- Collects job details such as title, position, educational level, experience, related fields, shift, and salary.
-- Adds the new job to the job list and saves it to the CSV file.
proposeVacancy :: [User] -> [Job] -> Recruiter -> [Application] -> IO ()
proposeVacancy userDataList jobDataList recruiter applications =
    putStrLn "Job Title: "  >> hFlush stdout >> getLine >>= \job ->
    putStrLn "Position: " >> hFlush stdout >> getLine >>= \position ->
    putStrLn "Educational Level requirement: " >> hFlush stdout >> getLine >>= \educationalLevel ->
    putStrLn "Experience: " >> hFlush stdout >> getLine >>= \experience ->
    putStrLn "Related Fields: " >> hFlush stdout >> getLine >>= \relatedFields ->
    putStrLn "Full time/Part time: " >> hFlush stdout >> getLine >>= \shift ->
    putStrLn "Salary: " >> hFlush stdout >> getLine >>= \salary ->

    pure (appendNewData jobDataList (pure (Job (getCompanyName recruiter) job position educationalLevel experience relatedFields shift salary))) >>= \updatedList ->
    putStrLn "Proposing job vacancy..." >> threadDelay 1500000  >> putStrLn "Job vacancy successfully proposed." >> threadDelay 1500000 >>
    writetoCSV "app/Job.csv" updatedList >>
    showRecruiterMenu userDataList jobDataList recruiter applications

-- Displays all applications submitted for the recruiter's job vacancies.
-- Filters applications based on the recruiter's company and displays details for each application.
checkApplicantJobApplication :: [User] -> [Job] -> Recruiter -> [Application] -> IO ()
checkApplicantJobApplication userDataList jobDataList recruiter applications = 
    pure (getCompanyName recruiter) >>= \recruiterCompany ->  
    pure (filter (\application ->
            getCompany (jobDetails application) == recruiterCompany) applications) >>= \matchedApplications ->
    if null matchedApplications
        then putStrLn "No applicants have applied for your job vacancies" >> threadDelay 1500000 >> showRecruiterMenu userDataList jobDataList recruiter applications
    else mapM_ displayApplication matchedApplications >>
        putStrLn "Press 0 to redirect back to main menu: " >>
        hFlush stdout >> getLine >>= \choice ->
            case choice of
                "0" -> showRecruiterMenu userDataList jobDataList recruiter applications
                _ -> putStrLn "Invalid input. Please enter again." >> checkApplicantJobApplication userDataList jobDataList recruiter applications
    where
        displayApplication :: Application -> IO ()
        displayApplication application = 
            pure (getJob (jobDetails application)) >>= \jobTitle ->   -- Fetch the job title from the application
            putStrLn ("Applying for job: " `mappend` jobTitle) >>
            putStrLn (displayApplicantInfo application) >>
            putStrLn "--------------------------------------------------"

-- Formats and displays the profile of an applicant from an application.
displayApplicantInfo :: Application -> String
displayApplicantInfo (Application applicantDetails _) =
    showProfile applicantDetails

-- Displays the profile of the logged-in recruiter.
-- Allows the recruiter to return to the main menu after viewing.
checkRecruiterProfile :: [User] -> [Job] -> Recruiter -> [Application] -> IO ()
checkRecruiterProfile userDataList jobDataList recruiter applications = 
    putStrLn (showProfile recruiter) >>
    putStrLn "\nEnter 0 to redirect back to main menu: " >>
    hFlush stdout >> getLine >>= \choice -> case choice of
        "0" -> showRecruiterMenu userDataList jobDataList recruiter applications
        _ -> putStrLn "Invalid input.Please try again..." >> threadDelay 1500000 >> clearscreen >> checkRecruiterProfile userDataList jobDataList recruiter applications

-- Allows the recruiter to edit their profile.
-- Offers options to update various fields like name, email, password, contact, company name, job title and position offered.
recruiterEditProfile :: [User] -> [Job] -> [Application] -> Recruiter -> IO ()
recruiterEditProfile userDataList jobDataList applications recruiter =
    putStrLn "Edit your profile?" >>
    putStrLn "1.Name\n2.Email\n3.Password\n4.Contact\n5.Company Name\n6.Job Title\n7.Position Offered\n8.Exit\nChoose an option (1-8):" >>
    hFlush stdout >> getLine >>= \choice -> case choice of
        "1" -> putStrLn "Please enter new name: " >> hFlush stdout >> getLine >>= \newName -> updateProfile setName newName recruiter userDataList
        "2" -> putStr "Please enter new " >> promptEmail >>= \newEmail -> 
            if isEmailTaken newEmail userDataList
                then putStrLn "Email exists. Enter a different email address." >> threadDelay 1500000 >> promptEmail >>= \newEmail -> updateProfile setEmail newEmail recruiter userDataList
                else updateProfile setEmail newEmail recruiter userDataList
        "3" -> putStrLn "Please enter new password: " >> hFlush stdout >> getLine >>= \newPassword -> updateProfile setPassword newPassword recruiter userDataList
        "4" -> putStr "Please enter new " >> promptContactNum >>= \newContactStr ->
                case reads newContactStr :: [(Int, String)] of
                    [(newContact, "")] -> updateProfile setContact newContact recruiter userDataList
                    _ -> putStrLn "Invalid input. Please enter a valid number." >> recruiterEditProfile userDataList jobDataList applications recruiter
        "5" -> putStrLn "Please enter new company name: " >> hFlush stdout >> getLine >>= \newCompanyName -> updateProfile setCompanyName newCompanyName recruiter userDataList
        "6" -> putStrLn "Please enter new job title: " >> hFlush stdout >> getLine >>= \newJobTitle -> updateProfile setJobTitle newJobTitle recruiter userDataList
        "7" -> putStrLn "Please enter new position offered: " >> hFlush stdout >> getLine >>= \newPosition -> updateProfile setPosition newPosition recruiter userDataList
        "8" -> showRecruiterMenu userDataList jobDataList recruiter applications
        _ -> putStrLn "Invalid choice. Please try again." >> threadDelay 1500000 >> clearscreen >> recruiterEditProfile userDataList jobDataList applications recruiter
    where
        updateProfile :: (a -> Recruiter -> Recruiter) -> a -> Recruiter ->  [User] -> IO ()
        updateProfile updateFunc newValue recruiter userDataList = 
            pure (updateFunc newValue recruiter) >>= \updatedRecruiter -> 
            pure (map (\user -> 
                case user of 
                    RecruiterUser a | recruiterEmail a == recruiterEmail recruiter && recruiterContact a == recruiterContact recruiter -> RecruiterUser updatedRecruiter
                    _ -> user) userDataList) >>= \updatedUserList ->
            writetoCSV "app/Users.csv" updatedUserList >>
            putStrLn "Changing profile..." >> threadDelay 1500000 >> putStrLn "Profile updated successfully!" >> threadDelay 1500000 >> 
            clearscreen >> recruiterEditProfile updatedUserList jobDataList applications updatedRecruiter

-- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--                                                                  Functions for Applicants
-- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Handles the login or sign-up process for applicants.
-- Displays a menu for the user to choose between logging in or signing up.
-- Based on the user's choice, redirects to the appropriate workflow.
applicantLoginOrSignup :: String -> RecruiterList -> ApplicantList -> [User] -> [Job] -> [Application] -> IO ()
applicantLoginOrSignup expectedRole recruiterList applicantList userDataList jobDataList applications =
    clearscreen >>
    putStrLn "Do you want to login or sign up?" >>
    putStrLn "1. Login" >>
    putStrLn "2. Sign Up" >> 
    putStrLn "Choose an option (1/2):" >> hFlush stdout >>
    getLine >>= \choice ->
        case choice of
            "1" -> clearscreen >> loginUser expectedRole recruiterList applicantList userDataList jobDataList applications
            "2" -> clearscreen >> showApplicantSignUpMenu applicantList userDataList jobDataList applications
            _ -> putStrLn "Invalid choice. Please try again..." >> threadDelay 1500000 >> applicantLoginOrSignup expectedRole recruiterList applicantList userDataList jobDataList applications

-- Handles the applicant sign-up process.
-- Collects details such as name, email, password, contact number, education level, course studied, and experience.
-- Validates the email to ensure it is unique, then creates a new applicant and saves it.
showApplicantSignUpMenu :: ApplicantList -> [User] -> [Job] -> [Application] -> IO ()
showApplicantSignUpMenu applicantList userDataList jobDataList applications =
    putStrLn "1. Name: " >> hFlush stdout >>
    getLine >>= \name ->
    putStr "2. " >> promptEmail >>= \email -> 
        if isEmailTaken email userDataList
            then putStrLn "The email already exist. Please try another email." >> threadDelay 1500000 >> clearscreen >> showApplicantSignUpMenu applicantList userDataList jobDataList applications
        else
            putStrLn "3. Password: " >> hFlush stdout >>
            getLine >>= \password ->
            putStr "4. " >> promptContactNum >>= \contactStr ->
            putStrLn "5. Level of Education: " >> hFlush stdout >>
            getLine >>= \education ->
            putStrLn "6. Course Studied: " >> hFlush stdout >>
            getLine >>= \course ->
            putStrLn "7. Experience: " >> hFlush stdout >>
            getLine >>= \experience ->
            -- Parse contact to Int
            pure (readContactStr contactStr) >>= \contact ->
            pure (Applicant name email password contact education course experience) >>= \newApplicant -> 
            pure (appendNewData userDataList [ApplicantUser newApplicant]) >>= \updatedUserList -> 
            writetoCSV "app/Users.csv" updatedUserList >>
            putStrLn "Sign Up Successful!" >> threadDelay 1500000 >>
            putStrLn ("Welcome " ++ name ++ "!") >> showApplicantMenu updatedUserList jobDataList applications newApplicant

-- Displays the main menu for applicants.
-- Offers options to search for jobs, check applications, remove applications, view profile, edit profile, or exit.
showApplicantMenu :: [User] -> [Job] -> [Application] -> Applicant -> IO ()
showApplicantMenu userDataList jobDataList applications applicant =
    clearscreen >>
    putStrLn "1. Search for Job" >>
    putStrLn "2. Check Application" >>
    putStrLn "3. Remove Application" >>
    putStrLn "4. Check Profile" >>
    putStrLn "5. Edit Profile" >>
    putStrLn "6. Exit" >>
    putStrLn "Choose an option (1-6):" >>
    hFlush stdout >> getLine >>= \choice ->
        case choice of
            "1" -> clearscreen >> searchForJob userDataList jobDataList applications applicant
            "2" -> clearscreen >> applicantJobApplication userDataList applicant jobDataList applications
            "3" -> clearscreen >> applicantRemoveJobApplication userDataList applicant jobDataList applications
            "4" -> clearscreen >> checkApplicantProfile userDataList jobDataList applications applicant
            "5" -> clearscreen >> applicantEditProfile userDataList jobDataList applications applicant
            "6" -> writetoCSV "app/Users.csv" userDataList
            _ -> putStrLn "Invalid choice. Please choose again." >> threadDelay 1500000 >> showApplicantMenu userDataList jobDataList applications applicant

-- Allows applicants to search for job vacancies.
-- Displays a list of available jobs and lets the applicant select one to apply for.
searchForJob :: [User] -> [Job] -> [Application] -> Applicant -> IO ()
searchForJob userDataList jobDataList applications applicant = 
    putStrLn "-------------------------------------------------" >>
    putStrLn "Job Applications: " >>
    if null jobDataList
        then putStrLn "No Job Vacancies Available..."
        else 
            putStrLn "-------------------------------------------------" >>
            mapM_ printJobWithNumber (zip [1..] jobDataList) >>
            putStrLn "\nEnter the job number to apply for the job or 0 to exit:" >>
            hFlush stdout >>
            userChoice jobDataList
  where
    printJobWithNumber (n, job) = 
        putStrLn ("Job #" `mappend` show n) >> 
        putStrLn (showVacancy job) >> 
        putStrLn "-------------------------------------------------"

    userChoice jobs =
        getLine >>= \choice ->
            case reads choice :: [(Int, String)] of
                [(0, "")] -> showApplicantMenu userDataList jobDataList applications applicant
                [(n, "")] | n > 0 && n <= length jobs ->
                    pure (jobs !! (n - 1)) >>= \jobSelection ->
                        pure (Application applicant jobSelection) >>= \newApplication ->
                        pure (appendNewData applications [newApplication]) >>= \updatedApplicationList ->
                        writetoCSV "app/Applications.csv" updatedApplicationList >>
                        putStrLn ("You have applied for:\n" `mappend` showVacancy jobSelection) >>
                        putStrLn "Thank you for applying." >> threadDelay 1500000 >> clearscreen >>
                        showApplicantMenu userDataList jobDataList updatedApplicationList applicant
                _ -> putStrLn "Invalid choice... Please enter a valid number.\nEnter the job number to apply for the job or 0 to exit:" >>
                     userChoice jobs

-- Displays all job applications made by the applicant.
applicantJobApplication :: [User] -> Applicant -> [Job] -> [Application] -> IO ()
applicantJobApplication userDataList applicant jobDataList applications = 
    pure (filter (\application -> 
            applicantEmail (applicantDetails application) == applicantEmail applicant) applications) >>= \matchedApplications -> 
    if null matchedApplications
        then putStrLn "You have not applied for any jobs." >> threadDelay 1500000 >>  showApplicantMenu userDataList jobDataList applications applicant
        else 
            putStrLn "Your applications: " *>
            putStrLn "-------------------------------------------------" *>
            mapM_ printApplicationsWithNumber (zip [1..] matchedApplications) *>
            putStrLn "Enter 0 to redirect back to main menu: " *>
            hFlush stdout >> getLine >>= \choice ->
                case choice of
                    "0" -> showApplicantMenu userDataList jobDataList applications applicant
                    _ -> putStrLn "Invalid input. Please enter again." >> threadDelay 1500000 >> applicantJobApplication userDataList applicant jobDataList applications
    where
        printApplicationsWithNumber (n, application) = 
            putStrLn ("Application #" `mappend` show n) *>
            putStrLn (showVacancy (jobDetails application)) *>
            putStrLn "-------------------------------------------------"

-- Allows applicants to remove a job application.
-- Displays the applicant's job applications and lets them select one to remove.
applicantRemoveJobApplication :: [User] -> Applicant -> [Job] -> [Application] -> IO ()
applicantRemoveJobApplication userDataList applicant jobDataList applications = 
            pure (filter (\application -> 
                    applicantEmail (applicantDetails application) == applicantEmail applicant) applications) >>= \matchedApplications -> 
            if null matchedApplications
                then putStrLn "You have not applied for any jobs." >> threadDelay 1500000 >> showApplicantMenu userDataList jobDataList applications applicant
                else 
                    putStrLn "Your applications: " *>
                    putStrLn "-------------------------------------------------" *>
                    mapM_ printApplicationsWithNumber (zip [1..] matchedApplications) *>
                    putStrLn "Enter the application number to remove the application or 0 to exit" *>
                    hFlush stdout *>
                    userChoice matchedApplications applications
            where
                printApplicationsWithNumber (n, application) = 
                    putStrLn ("Application #" `mappend` show n) *>
                    putStrLn (showVacancy (jobDetails application)) *>
                    putStrLn "-------------------------------------------------"

                userChoice applications currentApplications =
                    getLine >>= \choice ->
                        case reads choice :: [(Int, String)] of
                            [(0, "")] -> showApplicantMenu userDataList jobDataList applications applicant
                            [(n, "")] | n > 0 && n <= length applications -> 
                                pure (applications !! (n - 1)) >>= \applicationToRemove -> 
                                pure (filter (/= applicationToRemove) currentApplications) >>= \updatedApplications -> 
                                writetoCSV "app/Applications.csv" updatedApplications *>
                                putStrLn ("Application removed: " `mappend` showVacancy (jobDetails applicationToRemove)) *>
                                threadDelay 1500000 *>
                                showApplicantMenu userDataList jobDataList updatedApplications applicant
                            _ -> putStrLn "Invalid choice... Please enter a valid number" >> threadDelay 1500000 >> userChoice applications currentApplications

-- Displays the applicant's profile, including their password.
checkApplicantProfile :: [User] -> [Job] -> [Application] -> Applicant -> IO ()
checkApplicantProfile userDataList jobDataList applications applicant = 
    putStrLn (showProfile applicant) >>
    putStrLn ("Password: " `mappend` getPassword applicant) >>
    putStrLn "\nEnter 0 to redirect back to main menu: " >>
    hFlush stdout >> getLine >>= \choice -> case choice of
        "0" -> showApplicantMenu userDataList jobDataList applications applicant
        _ -> putStrLn "Invalid input.Please try again..." >> threadDelay 1500000 >> clearscreen >> checkApplicantProfile userDataList jobDataList applications applicant

-- Allows the applicant to edit their profile.
-- Offers options to update various fields like name, email, password, contact, education level, course studied, and experience.
applicantEditProfile :: [User] -> [Job] -> [Application] -> Applicant -> IO ()
applicantEditProfile userDataList jobDataList applications applicant =
    putStrLn "Edit your profile?" >>
    putStrLn "1.Name\n2.Email\n3.Password\n4.Contact\n5.Level of Education\n6.Course Studied\n7.Experience\n8.Exit\nChoose an option (1-8):" >>
    hFlush stdout >> getLine >>= \choice -> case choice of
        "1" -> putStrLn "Please enter new name: " >> hFlush stdout >> getLine >>= \newName -> updateProfile setName newName applicant userDataList
        "2" -> putStr "Please enter new " >> promptEmail >>= \newEmail -> 
            if isEmailTaken newEmail userDataList
                then putStrLn "Email already taken. Please choose another one." >> threadDelay 1500000 >> promptEmail >>= \newEmail -> updateProfile setEmail newEmail applicant userDataList
                else updateProfile setEmail newEmail applicant userDataList
        "3" -> putStrLn "Please enter new password: " >> hFlush stdout >> getLine >>= \newPassword -> updateProfile setPassword newPassword applicant userDataList
        "4" -> putStr "Please enter new " >> promptContactNum >>= \newContactStr ->
                case reads newContactStr :: [(Int, String)] of
                    [(newContact, "")] -> updateProfile setContact newContact applicant userDataList
                    _ -> putStrLn "Invalid input. Please enter a valid number." >> applicantEditProfile userDataList jobDataList applications applicant
        "5" -> putStrLn "Please enter new Level of Education: " >> hFlush stdout >> getLine >>= \newLevelOfEducation -> updateProfile setLevelOfEducation newLevelOfEducation applicant userDataList
        "6" -> putStrLn "Please enter new course studied: " >> hFlush stdout >> getLine >>= \newCourseStudied -> updateProfile setCourseStudied newCourseStudied applicant userDataList
        "7" -> putStrLn "Please enter new experience: " >> hFlush stdout >> getLine >>= \newExperience -> updateProfile setExperience newExperience applicant userDataList
        "8" -> showApplicantMenu userDataList jobDataList applications applicant
        _ -> putStrLn "Invalid choice. Please try again." >> threadDelay 1500000 >> clearscreen >> applicantEditProfile userDataList jobDataList applications applicant
    where
        -- Helper function to update a specific profile field.
        updateProfile :: (a -> Applicant -> Applicant) -> a -> Applicant ->  [User] -> IO ()
        updateProfile updateFunc newValue applicant userList = 
            pure (updateFunc newValue applicant) >>= \updatedApplicant ->
            pure (map (\user -> case user of 
                    ApplicantUser a | applicantEmail a == applicantEmail applicant && applicantContact a == applicantContact applicant -> ApplicantUser updatedApplicant 
                    _ -> user) userList) >>= \updatedUserList -> 
            writetoCSV "app/Users.csv" updatedUserList *>
            putStrLn "Changing profile..." >> threadDelay 1500000 >> putStrLn "Profile updated successfully!" >> threadDelay 1500000 >> 
            clearscreen >> applicantEditProfile updatedUserList jobDataList applications updatedApplicant

-- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--                                                                  Main Menu
-- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Prints ASCII art and branding for the system.
printAsciiArt :: IO ()
printAsciiArt = 
    clearscreen >>
    setSGR [SetColor Foreground Vivid Cyan] >>
    putStrLn (unlines [
        "     ▄▄▄▄▄▄▄     ▄▄▄▄▄▄▄     ▄▄▄▄▄▄▄     ▄▄▄▄▄▄▄     ▄▄▄▄▄▄▄     ▄▄▄▄▄▄▄   ",
        "    █  ▄ ▄  █   █  ▄ ▄  █   █  ▄ ▄  █   █  ▄ ▄  █   █  ▄ ▄  █   █  ▄ ▄  █  ",
        "   ██   ▄   ██ ██   ▄   ██ ██   ▄   ██ ██   ▄   ██ ██   ▄   ██ ██   ▄   ██ ",
        "   ██  █▄█  ██ ██  █▄█  ██ ██  █ █  ██ ██  █▄█  ██ ██  █ █  ██ ██  █ █  ██ ",
        "   ██       ██ ██       ██ ██  █▄█  ██ ██       ██ ██  █▄█  ██ ██  █▄█  ██ ",
        "    █   ▄   █   █   ▄   █   █       █   █   ▄   █   █       █   █       █  ",
        "    █▄▄█ █▄▄█   █▄▄█ █▄▄█   █▄▄█ █▄▄█   █▄▄█ █▄▄█   █▄▄█ █▄▄█   █▄▄█ █▄▄█  ",
        "                                                                           ",
        "    █████████████████████████████████████████████████████████████████████  ",
        "    █                                                                   █  ",
        "    █           ╔═╗╦═╗╦╔═╗   ╔═╗╔╗╔╔═╗╦╦  ╦   ╔═╗╔╗╔╔═╗╔═╗╦╦═╗          █  ",
        "    █           ╠═╝╠╦╝║╠═╝───║ ║║║║║ ╦║║  ║───║╣ ║║║║╣ ╠═╣║╠╦╝          █  ",
        "    █           ╩  ╩╚═╩╩     ╚═╝╝╚╝╚═╝╩╩═╝╩═╝ ╚═╝╝╚╝╚═╝╩ ╩╩╩╚═          █  ",
        "    █                                                                   █  ",
        "    █████████████████████████████████████████████████████████████████████  ",        
        "            Simplifying Your Career Path - Job Application System          ",
        "                    ╔══════════════════════════════════╗                   ",
        "                    ║      APPLY | GROW | SUCCEED      ║                   ",
        "                    ╚══════════════════════════════════╝                   "
    ]) >>
    setSGR [Reset]

-- Displays the main menu of the system.
-- Allows the user to choose between recruiter, applicant, or exit.
mainmenu :: RecruiterList -> ApplicantList -> [User] -> [Job] -> [Application] -> IO ()
mainmenu recruiterList applicantList userDataList jobDataList applications =
    printAsciiArt >>
    putStrLn "Are you a recruiter or applicant?" *>
    putStrLn "1. Recruiter" *>
    putStrLn "2. Applicant" *>
    putStrLn "3. Exit" *>
    putStrLn "Choose an option (1-3):" *>
    getLine >>= \choice ->
        case choice of
            "1" -> recruiterLoginOrSignup "Recruiter" recruiterList applicantList userDataList jobDataList applications
            "2" -> applicantLoginOrSignup "Applicant" recruiterList applicantList userDataList jobDataList applications
            "3" -> putStrLn "Exiting..." >> threadDelay 1000000 >> clearscreen
            _   -> putStrLn "Invalid choice.Please try again..." >> threadDelay 1500000 >> mainmenu recruiterList applicantList userDataList jobDataList applications

-- The entry point of the system.
-- Initializes the recruiter and applicant lists, reads data from CSV files, and displays the main menu.
main :: IO ()
main = 
    clearscreen >>
    initRecruiterList >>= \recruiterList ->
    initApplicantList >>= \applicantList -> 
    pure "app/Users.csv" >>= \userFilePath ->
    pure "app/Job.csv" >>= \jobFilePath ->
    pure "app/Applications.csv" >>= \applicationsFilePath ->

    readCSV userFilePath >>= \userDataList -> 
    readCSV jobFilePath >>= \jobDataList ->
    readCSV applicationsFilePath >>= \applications ->
        mainmenu recruiterList applicantList userDataList jobDataList applications