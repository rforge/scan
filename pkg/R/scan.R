.onAttach <- function(lib, pkg, ...) {
	out <- paste0("scan ",utils::packageVersion("scan")," (development version, ",utils::packageDate('scan'),")\n", #", packageDate("scan"), ")\n",
	              "Single-Case Data Analysis for Single and Multiple Baseline Designs\n")
	packageStartupMessage(out)
}	

.onLoad <- function(lib, pkg, ...) {}

#defaultAttributesSCDF <- function(...) {.defaultAttributesSCDF(...)}  

.male.names <- c("Jacob", "Mason", "Ethan", "Noah", "William", "Liam", "Jayden", "Michael", "Alexander", "Aiden", "Daniel", "Matthew", "Elijah", "James", "Anthony", "Benjamin", "Joshua", "Andrew", "David", "Joseph", "Logan", "Jackson", "Christopher", "Gabriel", "Samuel", "Ryan", "Lucas", "John", "Nathan", "Isaac", "Dylan", "Caleb", "Christian", "Landon", "Jonathan", "Carter", "Luke", "Owen", "Brayden", "Gavin", "Wyatt", "Isaiah", "Henry", "Eli", "Hunter", "Jack", "Evan", "Jordan", "Nicholas", "Tyler", "Aaron", "Jeremiah", 
                 "Julian", "Cameron", "Levi", "Brandon", "Angel", "Austin", "Connor", "Adrian", "Robert", "Charles", "Thomas", "Sebastian", "Colton", "Jaxon", "Kevin", "Zachary", "Ayden", "Dominic", "Blake", "Jose", "Oliver", "Justin", "Bentley", "Jason", "Chase", "Ian", "Josiah", "Parker", "Xavier", "Adam", "Cooper", "Nathaniel", "Grayson", "Jace", "Carson", "Nolan", "Tristan", "Luis", "Brody", "Juan", "Hudson", "Bryson", "Carlos", "Easton", "Damian", "Alex", "Kayden", "Ryder", "Jesus", "Cole", "Micah", "Vincent", "Max", "Jaxson", "Eric", "Asher", "Hayden", "Diego", "Miles", "Steven", "Ivan", "Elias", "Aidan", "Maxwell", "Bryce", "Antonio", "Giovanni", "Timothy", "Bryan", "Santiago", "Colin", "Richard", "Braxton", "Kaleb", "Kyle", "Kaden", "Preston", "Miguel", "Jonah", "Lincoln", "Riley", "Leo", "Victor", "Brady", "Jeremy", "Mateo", "Brian", "Jaden", "Ashton", "Patrick", "Declan", "Sean", "Joel", "Gael", "Sawyer", "Alejandro", "Marcus", "Leonardo", "Jesse", "Caden", "Jake", "Kaiden", "Wesley", "Camden", "Edward", "Brantley", "Roman", "Axel", "Silas", "Jude", "Grant", "Cayden", "Emmanuel", "George", "Maddox", "Malachi", "Bradley", "Alan", "Weston", "Gage", "Devin", "Greyson", "Kenneth", "Mark", "Oscar", "Tanner", "Rylan", "Nicolas", "Harrison", "Derek", "Peyton", "Ezra", "Tucker", "Emmett", "Avery", "Cody", "Calvin", "Andres", "Jorge", "Abel", "Paul", "Abraham", "Kai", "Collin", "Theodore", "Ezekiel", "Omar", "Jayce", "Conner", "Bennett", "Trevor", "Eduardo", "Peter", "Maximus", "Jaiden", "Jameson", "Seth", "Kingston", "Javier", "Travis", "Garrett", "Everett", "Graham", "Xander", "Cristian", "Damien", "Ryker", "Griffin", "Corbin", "Myles", "Luca", "Zane", "Francisco", "Ricardo", "Alexis", "Stephen", "Zayden", "Iker", "Drake", "Lukas", "Charlie", "Spencer", "Zion", "Erick", "Josue", "Jeffrey", "Trenton", "Chance", "Paxton", "Elliot", "Fernando", "Keegan", "Landen", "Manuel", "Amir", "Shane", "Raymond", "Zander", "Andre", "Israel", "Mario", "Cesar", "Simon", "King", "Jaylen", "Johnathan", "Troy", "Dean", "Clayton", "Dominick", "Tyson", "Jasper", "Martin", "Kyler", "Hector", 
                 "Edgar", "Marco", "Cash", "Edwin", "Shawn", "Judah", "Andy", "Donovan", "Kameron", "Elliott", "Dante", "Braylon", "Anderson", "Johnny", "Drew", "Sergio", "Cruz", "Dalton", "Rafael", "Gregory", "Lane", "Erik", "Skyler", "Finn", "Reid", "Gunner", "Jared", "Caiden", "Holden", "Emilio", "Fabian", "Aden", "Brendan", "Rowan", "Emiliano", "Braden", "Jase", "Jax", "Emanuel", "Lorenzo", "Roberto", "Amari", "Angelo", "Beau", "Louis", "Derrick", "Beckett", "Dawson", "Felix", "Pedro", "Brennan", "Frank", "Maximiliano", 
                 "Quinn", "Dallas", "Romeo", "Braylen", "Joaquin", "Waylon", "Allen", "Colt", "Ruben", "Milo", "Julius", "Grady", "August", "Dakota", "Cohen", "Brock", "Kellen", "Brycen", "Desmond", "Malik", "Colby", "Nehemiah", "Leland", "Jett", "Marcos", "Taylor", "Karter", "Marshall", "Ty", "Phillip", "Corey", "Ali", "Adan", "Dillon", "Arthur", "Maverick", "Leon", "Brooks", "Tristen", "Titus", "Keith", "Dexter", "Karson", "Emerson", "Landyn", "Armando", "Pablo", "Knox", "Enrique", "Cade", "Gerardo", "Reed", "Kellan", 
                 "Jayson", "Barrett", "Walter", "Dustin", "Kolton", "Ronald", "Trent", "Phoenix", "Ismael", "Julio", "Danny", "Kason", "Scott", "Messiah", "Jay", "Esteban", "Gideon", "Tate", "Abram", "Trey", "Keaton", "Jakob", "Jaime", "Devon", "Braydon", "Izaiah", "Donald", "Albert", "Raul", "Darius", "Archer", "Colten", "Damon", "River", "Gustavo", "Philip", "Atticus", "Walker", "Matteo", "Randy", "Saul", "Rocco", "Davis", "Enzo", "Noel", "Orion", "Jamari", "Remington", "Bruce", "Darren", "Larry", "Mathew", "Russell", "Dennis", "Tony", "Chris", "Porter", "Rodrigo", "Armani", "Zaiden", "Kade", "Ari", "Hugo", "Zachariah", "Kamden", "Mohamed", "Quentin", "Solomon", "Curtis", "Leonel", "Issac", "Khalil", "Alberto", "Jerry", "Alec", "Gianni", "Moises", "Gunnar", "Adriel", "Lawrence", "Alijah", "Chandler", "Ronan", "Prince", "Payton", "Arturo", "Jimmy", "Orlando", "Ricky", "Mitchell", "Maximilian", "Cason", "Malcolm", "Muhammad", "Kasen", "Marvin", "Jalen", "Cyrus", "Mauricio", "Warren", "Jonas", "Kendrick", "Rhys", "Dane", "Ryland", "Pierce", "Johan", "Rory", "Uriel", "Major", "Bryant", "Reece", "Casey", "Ibrahim", "Nikolas", "Arjun", "Sullivan", "Finnegan", "Alfredo", "Royce", "Ahmed", "Amare", "Lance", "Ramon", "Jamison", "Brayan", "Brenden", "Dominik", "Case", "Kristopher", "Maurice", "Mekhi", "Kobe", "Zackary", "Rhett", "Jensen", "Jaxton", "Deandre", "Isaias", "Channing", "Yahir", "Ezequiel", "Tobias", "Talon", "Sam", "Justice", "Kash", "Nash", "Alvin", "Jacoby", "Ace", "Nico", "Quinton", "Cannon", "Franklin", "Raiden", "Joe", "Lawson", "Beckham", "Gary", "Aldo", "Raylan", "Frederick", "London", "Boston", "Carl", "Byron", "Ernesto", "Moshe", "Terry", "Eddie", "Kane", "Moses", "Finley", "Salvador", "Reese", "Kelvin", "Cullen", "Madden", "Wade", "Clark", "Mohammed", "Kieran", "Jagger", "Dorian", "Korbin", "Nelson", "Roy", "Asa", "Matias", "Nasir", "Nickolas", "Roger", "Alonzo", "Jaxen", "Skylar", "Callen", "Malakai", "Douglas", "Ahmad", "Uriah", "Conor", "Kristian", "Carmelo", "Blaine", "Kayson", "Bentlee", "Braeden", "Julien", "Nathanael", "Aarav", "Keagan", "Lucian", "Morgan", "Chad", "Terrance", "Benson", "Noe", "Rodney", "Francis", "Layne", "Mohammad", "Zayne", "Tatum", "Brett", "Wilson", "Kian", "Marc", "Rohan", "Dayton", "Braiden", "Harper", "Luciano", "Nikolai", "Kamari", "Camron", "Joey", "Santino", "Ellis", "Layton", "Xzavier", "Jefferson", "Winston", "Guillermo", "Demetrius", "Bowen", "Daxton", "Melvin", "Soren", "Neil", "Sylas", "Jon", "Raphael", "Rex", "Yusuf", "Shaun", "Brodie", "Tommy", "Harley", "Quincy", "Dax", "Trace", "Adonis", "Bently", "Giovani", "Jeffery", 
                 "Odin", "Luka", "Kylan", "Willie", "Lewis", "Tripp", "Vihaan", "Davion", "Kendall", "Arian", "Cory", "Jamarion", "Jonathon", "Nixon", "Rayan", "Emery", "Jermaine", "Reginald", "Tomas", "Emmitt", "Ayaan", "Zechariah", "Billy", "Hamza", "Micheal", "Urijah",
                 "Aryan", "Lee", "Jasiah", "Landry", "Crosby", "Mathias", "Toby", "Tristian", "Will", "Felipe", "Triston", "Eden", "Terrell", "Deacon", "Matthias", "Jamal", "Makai", "Maxim", "Sterling", "Hank", "Gerald", "Alessandro", "Jaydon", "Hayes", "Niko", "Branson", 
                 "Flynn", "Kody", "Marlon", "Mayson", "Allan", "Augustus", "Jessie", "Neymar", "Adrien", "Aydan", "Leonard", "Sincere", "Kyson", "Terrence", "Jerome", "Jadiel", "Kole", "Aron", "Aydin", "Omari", "Ronnie", "Zain", "Vicente", "Bobby", "Yosef", "Alexzander", 
                 "Harry", "Kale", "Rogelio", "Casen", "Ray", "Clay", "Masen", "Sage", "Ulises", "Kymani", "Chaim", "Javon", "Brent", "Jadon", "Elisha", "Stanley", "Jovanni", "Princeton", "Alonso", "Darian", "Conrad", "Dwayne", "Eugene", "Gauge", "Rene", "Kareem", "Roland", "Ben", "Vincenzo", "Abdullah", "Camren", "Kenny", "Brentley", "Memphis", "Blaze", "Edison", "Osvaldo", "Teagan", "Westin", "Deshawn", "Rayden", "Cedric", "Marquis", "Samir", "Steve", "Draven", "Jairo", "Giovanny", "Brennen", "Bronson", "Crew", "Davin", "Kolten", "Ronin", "Ariel", "Semaj", "Alden", "Isiah", "Lennox", "Davian", "Jaylin", "Cain", "Wayne", "Craig", "Lamar", "Leonidas", "Cristopher", "Otto", "Bo", "Darrell", "Kolby", "Marcelo", "Bruno", "Fletcher", "Justus", "Alfonso", "Theo", "Tyrone", "Aidyn", "Harvey", "Rudy", "Brendon", "Tristin", "Dominique", "Kaeden", "Samson", "Kyree", "Jovani", "Lionel", "Amos", "Giancarlo", "Misael", "Callum", "Quintin", "Valentino", "Gavyn", "Lennon", "Jamir", "Kamron", "Zavier", "Arlo", "Junior", "Killian", "Leandro", "Konnor", "Hezekiah", "Jordyn", "Markus", "Ramiro", "Callan", "Chace", "Johnathon", "Lyric", "Fisher", "Rashad", "Kamryn", "Legend", "Duncan", "Harold", "Camilo", "Hendrix", "Seamus", "Coleman", "Vance", "Rylee", "Elian", "Jaeden", "Jamie", "Krish", "Abdiel", "Antoine", "Camdyn", "Van", "Branden", "Cayson", "Gibson", "Javion", "Izayah", "Darwin", "Jamar", "Mike", "Randall", "Brecken", "Hassan", "Thiago", "Heath", "Arnav", "Kingsley", "Kyrie", "Xavi", "Damari", "Deangelo", "Jionni", "Joziah", "Makhi", "Vaughn", "Zeke", "Konner", "Ean", "Frankie", "Yael", "Benton", "Oakley", "Efrain", "Marcel", "Rolando", "Maxton", "Jaycob", "Keenan", "Rowen", "Yousef", "Ishaan", "Jedidiah", "Remy", "Todd", "Reagan", "Bodhi", "Damarion", "Juelz", "Valentin", "Austyn", "Broderick", "Anders", "Alvaro", "Mustafa", "Thaddeus", "Brenton", "Cale", "Clinton", "Derick", "Jorden", "Gilberto", "Jabari", "Rey", "Salvatore", "Freddy", "Donte", "Ernest", "Aaden", "Axton", "Blaise", "Lucca", "Maximo", "Sidney", "Dario", "Rodolfo", "Trevon", "Camryn", "Deegan", "Sonny", "Cassius", "Truman", "Brice", "Brogan", "Hugh", "Yehuda", "Agustin", "Eliot", "Stefan", "Zaid", "Bridger", "Damion", "Eliseo", "Houston", "Johann", "Leroy", "Sheldon", "Dariel", "Darryl", "Isai", "Tyrell", "Alfred", "Demarcus", "Kohen", "Ignacio", "Rylen", "Santos", "Cael", "Davon", "Kaysen", "Mack", "Darien", "Ross", "Titan", "Tyree", "Ameer", "Zaire", "Aditya", "Briggs", "Immanuel", "Malaki", "Turner", "Bradyn", "Graysen", "Kase", "Reuben", "Yandel", "Gaige", "Jaidyn", "Franco", "Trystan", "Maison", "Simeon", "Anton", 
                 "Darnell", "Emory", "Roderick", "Deon", "Devan", "Graeme", "Howard", "Jael", "Kael", "Karsen", "Jarrett", "Apollo", "Denzel", "Foster", "Gilbert", "Jaylon", "Kylen", "Augustine")	

.female.names <- c("Sophia", "Emma", "Isabella", "Olivia", "Ava", "Emily", "Abigail", "Mia", "Madison", "Elizabeth", "Chloe", "Ella", "Avery", "Addison", "Aubrey", "Lily", "Natalie", "Sofia", "Charlotte", "Zoey", "Grace", "Hannah", "Amelia", "Harper", "Lillian", "Samantha", "Evelyn", "Victoria", "Brooklyn", "Zoe", "Layla", "Hailey", "Leah", "Kaylee", "Anna", "Aaliyah", "Gabriella", "Allison", "Nevaeh", "Alexis", "Audrey", "Savannah", "Sarah", "Alyssa", "Claire", "Taylor", "Riley", "Camila", "Arianna", "Ashley", "Brianna", "Sophie", "Peyton", "Bella", "Khloe", "Genesis", "Alexa", "Serenity", "Kylie", "Aubree", "Scarlett", "Stella", "Maya", "Katherine", "Julia", "Lucy", "Madelyn", "Autumn", "Makayla", "Kayla", "Mackenzie", "Lauren", "Gianna", "Ariana", "Faith", "Alexandra", "Melanie", "Sydney", "Bailey", "Caroline", "Naomi", "Morgan", "Kennedy", "Ellie", "Jasmine", "Eva", "Skylar", "Kimberly", "Violet", "Molly", "Aria", "Jocelyn", "Trinity", "London", "Lydia", "Madeline", "Reagan", "Piper", "Andrea", "Annabelle", "Maria", "Brooke", "Payton", "Paisley", "Paige", "Ruby", "Nora", "Mariah", "Rylee", "Lilly", "Brielle", "Jade", "Destiny", "Nicole", "Mila", "Kendall", "Liliana", "Kaitlyn", "Natalia", "Sadie", "Jordyn", "Vanessa", "Mary", "Mya", "Penelope", "Isabelle", "Alice", "Reese", "Gabrielle", "Hadley", "Katelyn", "Angelina", "Rachel", "Isabel", "Eleanor", "Clara", "Brooklynn", "Jessica", "Elena", "Aliyah", "Vivian", "Laila", "Sara", "Amy", "Eliana", "Lyla", "Juliana", "Valeria", "Adriana", "Makenzie", "Elise", "Mckenzie", "Quinn", "Delilah", "Cora", "Kylee", "Rebecca", "Gracie", "Izabella", "Josephine", "Alaina", "Michelle", "Jennifer", "Eden", "Valentina", "Aurora", "Catherine", "Stephanie", "Valerie", "Jayla", "Willow", "Daisy", "Alana", "Melody", "Hazel", "Summer", "Melissa", "Margaret", "Kinsley", "Kinley", "Ariel", "Lila", "Giselle", "Ryleigh", "Haley", "Julianna", "Ivy", "Alivia", "Brynn", "Keira", "Daniela", "Aniyah", "Angela", "Kate", "Londyn", "Hayden", "Harmony", "Adalyn", "Megan", "Allie", "Gabriela", "Alayna", "Presley", "Jenna", "Alexandria", "Ashlyn", "Adrianna", "Jada", "Fiona", "Norah", "Emery", "Maci", "Miranda", "Ximena", "Amaya", "Cecilia", "Ana", "Shelby", "Katie", "Hope", "Callie", "Jordan", "Luna", "Leilani", "Eliza", "Mckenna", "Angel", "Genevieve", "Makenna", "Isla", "Lola", "Danielle", "Chelsea", "Leila", "Tessa", "Adelyn", "Camille", "Mikayla", "Adeline", "Adalynn", "Sienna", "Esther", "Jacqueline", "Emerson", "Arabella", "Maggie", "Athena", "Lucia", "Lexi", "Ayla", "Diana", "Alexia", "Juliet", "Josie", "Allyson", "Addyson", "Delaney", "Teagan", "Marley",
                   "Amber", "Rose", "Erin", "Leslie", "Kayleigh", "Amanda", "Kathryn", "Kelsey", "Emilia", "Alina", "Kenzie", "Kaydence", "Alicia", "Alison", "Paris", "Sabrina", "Ashlynn", "Lilliana", "Sierra", "Cassidy", "Laura", "Alondra", "Iris", "Kyla", "Christina", "Carly", "Jillian", "Madilyn", "Kyleigh", "Madeleine", "Cadence", "Nina", "Evangeline", "Nadia", "Raegan", "Lyric", "Giuliana", "Briana", "Georgia", "Yaretzi", "Elliana", "Haylee", "Fatima", "Phoebe", "Selena", "Charlie", "Dakota", "Annabella", "Abby", "Daniella", "Juliette", "Lilah", "Bianca", "Mariana", "Miriam", "Parker", "Veronica", "Gemma", "Noelle", "Cheyenne", "Marissa", "Heaven", "Vivienne", "Brynlee", "Joanna", "Mallory", "Aubrie", "Journey", "Nyla", "Cali", "Tatum", "Carmen", "Gia", "Jazmine", "Heidi", "Miley", "Baylee", "Elaina", "Macy", "Ainsley", "Jane", "Raelynn", "Anastasia", "Adelaide", "Ruth", "Camryn", "Kiara", "Alessandra", "Hanna", "Finley", "Maddison", "Lia", "Bethany", "Karen", "Kelly", "Malia", "Jazmin", "Jayda", "Esmeralda", "Kira", "Lena", "Kamryn", "Kamila", "Karina", "Eloise", "Kara", "Elisa", "Rylie", "Olive", "Nayeli", "Tiffany", "Macie", "Skyler", "Addisyn", "Angelica", "Briella", "Fernanda", "Annie", "Maliyah", "Amiyah", "Jayden", "Charlee", "Caitlyn", "Elle", "Crystal", "Julie", "Imani", "Kendra", "Talia", "Angelique", "Jazlyn", "Guadalupe", "Alejandra", "Emely", "Lucille", "Anya", "April", "Elsie", "Madelynn", "Myla", "Julissa", "Scarlet", "Helen", "Breanna", "Kyra", "Madisyn", "Rosalie", "Brittany", "Brylee", "Jayleen", "Arielle", "Karla", "Kailey", "Arya", "Sarai", "Harley", "Miracle", "Kaelyn", "Kali", "Cynthia", "Daphne", "Aleah", "Caitlin", "Cassandra", "Holly", "Janelle", "Marilyn", "Katelynn", "Kaylie", "Itzel", "Carolina", "Bristol", "Haven", "Michaela", "Monica", "June", "Janiyah", "Camilla", "Jamie", "Rebekah", "Audrina", "Dayana", "Lana", "Serena", "Tiana", "Nylah", "Braelyn", "Savanna", "Skye", "Raelyn", "Madalyn", "Sasha", "Perla", "Bridget", "Aniya", "Rowan", "Logan", "Mckinley", "Averie", "Jaylah", "Aylin", "Joselyn", "Nia", "Hayley", "Lilian", "Adelynn", "Jaliyah", "Kassidy", "Kaylin", "Kadence", "Celeste", "Jaelyn", "Zariah", "Tatiana", "Jimena", "Lilyana", "Anaya", "Catalina", "Viviana", "Cataleya", "Sloane", "Courtney", "Johanna", "Amari", "Melany", "Anabelle", "Francesca", "Ada", "Alanna", "Priscilla", "Danna", "Angie", "Kailyn", "Lacey", "Sage", "Lillie", "Brinley", "Caylee", "Joy", "Kenley", "Vera", "Bailee", "Amira", "Aileen", "Aspen", "Emmalyn", "Erica", "Gracelyn", "Kennedi", "Skyla", "Annalise", "Danica", "Dylan", "Kiley", "Gwendolyn", "Jasmin", "Lauryn", 
                   "Aleena", "Justice", "Annabel", "Tenley", "Dahlia", "Gloria", "Lexie", "Lindsey", "Hallie", "Sylvia", "Elyse", "Annika", "Maeve", "Marlee", "Aryanna", "Kenya", "Lorelei", "Selah", "Kaliyah", "Adele", "Natasha", "Brenda", "Erika", "Alyson", "Braylee", "Emilee", "Raven", "Ariella", "Blakely", "Liana", "Jaycee", "Sawyer", "Anahi", "Jaelynn", "Elsa", "Farrah", "Cameron", "Evelynn", "Luciana", "Zara", "Madilynn", "Eve", "Kaia", "Helena", "Anne", "Estrella", "Leighton", "Nataly", "Whitney", "Lainey", "Amara", "Anabella", "Malaysia", "Samara", "Zoie", "Amani", "Phoenix", "Dulce", "Paola", "Marie", "Aisha", "Harlow", "Virginia", "Ember", "Regina", "Jaylee", "Anika", "Ally", "Kayden", "Alani", "Miah", "Yareli", "Journee", "Kiera", "Nathalie", "Mikaela", "Jaylynn", "Litzy", "Charley", "Claudia", "Aliya", "Madyson", "Cecelia", "Liberty", "Braelynn", "Evie", "Rosemary", "Myah", "Lizbeth", "Giana", "Ryan", "Teresa", "Ciara", "Isis", "Lea", "Shayla", "Jazlynn", "Rosa", "Gracelynn", "Desiree", "Elisabeth", "Isabela", "Arely", "Mariam", "Abbigail", "Emersyn", "Brenna", "Kaylynn", "Nova", "Raquel", "Dana", "Laney", "Laylah", "Siena", "Amelie", "Clarissa", "Lilianna", "Lylah", "Halle", "Madalynn", "Maleah", "Sherlyn", "Linda", "Shiloh", "Jessie", "Kenia", "Greta", "Marina", "Melina", "Amiya", "Bria", "Natalee", "Sariah", "Mollie", "Nancy", "Christine", "Felicity", "Zuri", "Irene", "Simone", "Amya", "Matilda", "Colette", "Kristen", "Paityn", "Alayah", "Janiya", 
                   "Kallie", "Mira", "Hailee", "Kathleen", "Meredith", "Janessa", "Noemi", "Aiyana", "Aliana", "Leia", "Mariyah", "Tori", "Alissa", "Ivanna", "Joslyn", "Sandra", "Maryam", "Saniyah", "Kassandra", "Danika", "Denise", "Jemma", "River", "Charleigh", "Emelia", "Kristina", "Armani", "Beatrice", "Jaylene", "Karlee", "Blake", "Cara", "Addilyn", "Amina", "Ansley", "Kaitlynn", "Iliana", "Mckayla", "Adelina", "Briley", "Elaine", "Lailah", "Mercedes", "Chaya", "Lindsay", "Hattie", "Lisa", "Marisol", "Patricia", "Bryanna", "Taliyah", "Adrienne", "Emmy", "Millie", "Paislee", "Charli", "Kourtney", "Leyla", "Maia", "Willa", "Milan", "Paula", "Ayleen", "Clare", "Kensley", "Reyna", "Martha", "Adley", "Elianna", "Emilie", "Karsyn", "Yasmin", "Lorelai", "Amirah", "Aryana", "Livia", "Alena", "Kiana", "Celia", "Kailee", "Rylan", "Ellen", "Galilea", "Kynlee", "Leanna", "Renata", "Mae", "Ayanna", "Chanel", "Lesly", "Cindy", "Carla", "Pearl", "Jaylin", "Kimora", "Angeline", "Carlee", "Aubri", "Edith", "Alia", "Frances", "Corinne", "Jocelynn", "Cherish", "Wendy", "Carolyn", "Lina", "Tabitha", "Winter", "Abril", "Bryn", "Jolie", "Yaritza", "Casey", "Zion", "Lillianna", "Jordynn", "Zariyah", "Audriana", "Jayde", "Jaida", "Salma", "Diamond", "Malaya", "Kimber", "Ryann", "Abbie", "Paloma", "Destinee", "Kaleigh", "Asia", "Demi", "Yamileth", "Deborah", "Elin", "Kaiya", "Mara", "Averi", "Nola", "Tara", "Taryn", "Emmalee", "Aubrianna", "Janae", "Kyndall", "Jewel", "Zaniyah", 
                   "Kaya", "Sonia", "Alaya", "Heather", "Nathaly", "Shannon", "Ariah", "Avah", "Giada", "Lilith", "Samiyah", "Sharon", "Coraline", "Eileen", "Julianne", "Milania", "Chana", "Regan", "Krystal", "Rihanna", "Sidney", "Hadassah", "Macey", "Mina", "Paulina", "Rayne", "Kaitlin", "Maritza", "Susan", "Raina", "Hana", "Keyla", "Temperance", "Aimee", "Alisson", "Charlize", "Kendal", "Lara", "Roselyn", "Alannah", "Alma", "Dixie", "Larissa", "Patience", "Taraji", "Sky", "Zaria", "Aleigha", "Alyvia", "Aviana", "Bryleigh", "Elliot", "Jenny", "Luz", "Ali", "Alisha", "Ayana", "Campbell", "Karis", "Lilyanna", "Azaria", "Blair", "Micah", "Moriah", "Myra", "Lilia", "Aliza", "Giovanna", "Karissa", "Saniya", "Emory", "Estella", "Juniper", "Kairi", "Kenna", "Meghan", "Abrielle", "Elissa", "Rachael", "Emmaline", "Jolene", "Joyce", "Britney", "Carlie", "Haylie", "Judith", "Renee", "Saanvi", "Yesenia", "Barbara", "Dallas", "Jaqueline", "Karma", "America", "Sariyah", "Azalea", "Everly", "Ingrid", "Lillyana", "Emmalynn", "Marianna", "Brisa", "Kaelynn", "Leona", "Libby", "Deanna", "Mattie", "Miya", "Kai", "Annalee", "Nahla", "Dorothy", "Kaylyn", "Rayna", "Araceli", "Cambria", "Evalyn", "Haleigh", "Thalia", "Jakayla", "Maliah", "Saige", "Avianna", "Charity", "Kaylen", "Raylee", "Tamia", "Aubrielle", "Bayleigh", "Carley", "Kailynn", "Katrina", "Belen", "Karlie", "Natalya", "Alaysia", "Celine", "Milana", "Monroe", "Estelle", "Meadow", "Audrianna", "Cristina", "Harlee", "Jazzlyn", "Scarlette", "Zahra", "Akira", "Ann", "Collins", "Kendyl", "Anabel", "Azariah", "Carissa", "Milena", "Tia", "Alisa", "Bree", "Carleigh", "Cheyanne", "Sarahi", "Laurel", "Kylah", "Tinley", "Kora", "Marisa", "Esme", "Sloan", "Cailyn", "Gisselle", "Kasey", "Kyndal", "Marlene", "Riya", "Annabell", "Aubriana", "Izabelle", "Kirsten", "Aya", "Dalilah", "Devyn", "Geraldine", "Analia", "Hayleigh", "Landry", "Sofie", "Tess", "Ashtyn", "Jessa", "Katalina")	

.names <- c(.male.names, .female.names)

.names <- .names[which(!duplicated(.names))]

.opt <- list(
  dv    = "var.values",
  phase = "var.phase",
  mt    = "var.mt",
  female.names = .female.names,
  male.names = .male.names,
  names = .names,
  function_debugging_warning = "This function is in an experimental state and only implemented for testing und debugging purposes.\n",
  function_deprecated_warning = "This function is deprecated. It will be droppoed without any further notice in a future update of scan.\n"
  
)

.defaultAttributesSCDF <- function(attri = NULL) {
  out <- list()
  if(!is.null(attri))
    out <- attri
  out$class <- c("scdf","list")
  out[.opt$phase] <- "phase"
  out[.opt$dv] <- "values"
  out[.opt$mt] <- "mt"
  out
}  

.SCmovingAverage <- function(x, xLag, FUN = mean) {
	for(i in (xLag + 1):(length(x) - xLag))
		x[i] <- FUN(x[(i - xLag):(i + xLag)], na.rm = TRUE)
	return(x)
}
	
.SCac <- function(x, lag = 1) {
	m <- mean(x, na.rm = TRUE)
	ax1 <- x[1:(length(x) - lag)]-m
	ax2 <- x[(lag + 1):length(x)]-m
	ac <- sum(ax1*ax2, na.rm = TRUE)/sum((x-m)^2, na.rm = TRUE)
	ac
}

.SClm <- function(x = NULL,y) {
	if(is.null(x))
		x <- 1:length(y)
	mx <- mean(x)
	my <- mean(y)
	ss.xy <- sum( (x-mx)*(y-my) )
	ss.xx <- sum( (x-mx)^2 )
	b <- ss.xy/ss.xx
	b
}

.SCbeta <- function(model) {
	  b <- model$coefficients[-1]
    sx <- apply(model$model[-1],2,sd)
    sy <- apply(model$model[1],2,sd)
    return(c(model$coefficients,b * sx/sy))
}

.phasestructure <- function(data,pvar) {
  phases <- rle(as.character(data[,pvar]))
  phases$start <- c(1, cumsum(phases$lengths) + 1)[1 : length(phases$lengths)]
  phases$stop <- cumsum(phases$lengths)
  class(phases) <- "list"
  return(phases)
}

.SCprepareData <- function(data, na.rm = FALSE, change.var.phase = TRUE, change.var.values = TRUE, change.var.mt = TRUE) {
  
  if(is.data.frame(data)) {
    data <- list(data)
    attributes(data) <- .defaultAttributesSCDF()
  }
  if(!is.list(data))
    stop("Wrong data format. Data must be a data frame or a list of data frames.")
  
  if(is.null(attr(data, .opt$phase)))
    attr(data,.opt$phase) <- "phase"
  if(is.null(attr(data, .opt$mt)))
    attr(data,.opt$mt)    <- "mt"
  if(is.null(attr(data, .opt$dv)))
    attr(data,.opt$dv)    <- "values"
  
  pvar       <- attr(data,.opt$phase)
  var.mt     <- attr(data, .opt$mt)
  var.values <- attr(data, .opt$dv)

  
  if(is.null(names(data)))
    names(data) <- paste0("Case",1:length(data))
  
  for(case in 1:length(data)) {
    VARS <- names(data[[case]])
    if(!(var.values %in% VARS))
      stop("No variable for values with the name ",var.values, " in the scdf.")
    if(!(pvar %in% VARS))
      stop("No variable for phase with the name ",pvar,  " in the scdf.")
    if(!(var.mt %in% VARS))
      stop("No variable for mt with the name ",var.mt,     " in the scdf.")
   
    if(na.rm)
      data[[case]] <- data[[case]][!is.na(data[[case]][, var.values]),]
    if(!is.factor(data[[case]][, pvar]))
      data[[case]][, pvar] <- as.factor(data[[case]][, pvar])
    
    
    if(change.var.values && var.values != "values") {
      if("values" %in% VARS) {
        warning("Original values variable was renamed to values_renamed for this analysis.")
        names(data[[case]])[match("values",VARS)] <- "values_renamed"
      }
      names(data[[case]])[match(var.values, VARS)] <- "values"
    }
    
    if(change.var.mt && !(var.mt %in% VARS)) {
      data[[case]][,var.mt] <- 1:nrow(data[[case]])
    }
    
    if(change.var.mt && var.mt != "mt") {
      if("mt" %in% VARS) {
        warning("Original mt variable was renamed to mt_renamed for this analysis.")
        names(data[[case]])[match("mt",VARS)] <- "mt_renamed"
      }
      names(data[[case]])[match(var.mt, VARS)] <- "mt"
    }
    
    if(change.var.phase && pvar != "phase") {
      if("phase" %in% VARS) {
        warning("Original phase variable was renamed to phase_renamed for this analysis.")
        names(data[[case]])[match("phase",VARS)] <- "phase_renamed"
      }
      names(data[[case]])[match(pvar, VARS)] <- "phase"
    }
    if(is.na(names(data)[case]))
      names(data)[case] <- paste0("Case ", case)
  }
  
  
  
  return(data)
}

#keepphasesSC <- function(...) {.keepphasesSC(...)}

.keepphasesSC <- function(data, phases = c(1,2), set.phases = TRUE, pvar = "phase") {
  
  if(is.data.frame(data))
    data <- list(data)
  
  ATTRIBUTES <- attributes(data)
  
  res <- lapply(data, function(x) rle(as.character(x[,pvar]))$values)
  if(!all(unlist(lapply(res[-1], function(x) identical(x,res[[1]])))))
    warning("Single-cases do have differing desings.")
  
  if (class(phases) %in% c("character","numeric","integer")) {
    if(!length(phases) == 2) 
      stop("Phases argument not set correctly. Please provide a vector with two charcters or two numbers. E.g., phases = c(1,3).")
    phases.A <- phases[1]
    phases.B <- phases[2]
  }
  
  if (class(phases) == "list") {
    phases.A <- phases[[1]]
    phases.B <- phases[[2]]
  }
  
  phases.total <- c(phases.A, phases.B)
  design <- rle(as.character(data[[1]][,pvar]))
  
  if(class(phases.total) == "character") {
    tmp <- sapply(phases.total, function(x) sum(x == design$values)>1)
    if(any(tmp))
      stop(paste0("Phase names ", paste0(names(tmp[tmp]))," occure several times. Please give number of phases instead of characters."))
    
    tmp <- sapply(phases.total, function(x) any(x == design$values))
    if(!all(tmp))
      stop(paste0("Phase names ",  names(tmp[!tmp]) ," do not occure in the data. Please give different phase names."))
  }
  
  if(class(phases.total) == "character") {
    phases.A <- which(design$values %in% phases.A)
    phases.B <- which(design$values %in% phases.B)
  }
  
  N <- length(data)
  design.list <- list()
  
  for(case in 1:N) {
    design <- rle(as.character(data[[case]][,pvar]))
    design$start <- c(1,cumsum(design$lengths)+1)[1:length(design$lengths)]
    design$stop <- cumsum(design$lengths)
    class(design) <- "list"
    
    A <- unlist(lapply(phases.A, function(x) design$start[x]:design$stop[x]))
    B <- unlist(lapply(phases.B, function(x) design$start[x]:design$stop[x]))
    
    data[[case]][,pvar] <- as.character(data[[case]][,pvar])
    
    if(set.phases) {
      data[[case]][A ,pvar] <- "A"
      data[[case]][B ,pvar] <- "B"
    }
    data[[case]] <- data[[case]][c(A,B),]
    design.list[[case]] <- design
  }
  attributes(data) <- ATTRIBUTES
  out <- list(data = data, designs = design.list, N = N, phases.A = phases.A, phases.B = phases.B)
  return(out)
}

.stringPhasesSC <- function(A,B) {
  nomerS = "phase "
  nomerP = "phases "
  
  APART <- 
    if(length(A) == 1)
      paste0(nomerS, A, collapse = "")
  else 
    paste0( c(nomerP, A[1], paste0(" + ",A[-1]) ), 
            collapse = "")
  
  BPART <- 
    if(length(B) == 1)
      paste0(nomerS, B, collapse = "")
  else 
    paste0( c(nomerP,B[1], paste0(" + ",B[-1])), 
            collapse = "")
  
  out <- paste0(c("Comparing ", APART, " against ", BPART), collapse ="")
  out
  
}



.onAttach()



