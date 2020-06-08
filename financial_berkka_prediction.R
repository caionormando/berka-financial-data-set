#Loading libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(modelr)
library(data.table)

#Dataset reading

fread("account.asc",
      sep = ";",
      dec = ",",
      header = TRUE) -> account
fread("card.asc",
      sep = ";",
      dec = ",",
      header = TRUE) -> card
fread("client.asc",
      sep = ";",
      dec = ",",
      header = TRUE) -> client
fread("disp.asc",
      sep = ";",
      dec = ",",
      header = TRUE) -> disposition
fread("district.asc",
      sep = ";",
      dec = ",",
      header = TRUE) -> district
fread("loan.asc",
      sep = ";",
      dec = ".",
      header = TRUE) -> loan
fread("order.asc",
      sep = ";",
      dec = ".",
      header = TRUE) -> order
fread("trans.asc",
      sep = ";",
      dec = ".",
      header = TRUE) -> transaction

#Account

# • Translate "frequency" column
#"POPLATEK MESICNE" stands for monthly
#issuance
#"POPLATEK TYDNE" stands for weekly
#issuance
#"POPLATEK PO OBRATU" stands for
#issuance after transaction

install.packages("stringr")
install.packages('lubridate')
library(lubridate)
library (stringr)

#Creating tibbles for each entity
tibble_account <- tibble(account) %>%
  mutate(frequency = str_replace_all(
    frequency,
    c(
      "POPLATEK MESICNE" = "Month Issuance",
      "POPLATEK TYDNE" = "Weekly issuance",
      "POPLATEK PO OBRATU" = "Issuance after transaction"
    )
  )) %>%
  mutate(date = ymd(date))



#Card
# • Removing timestamp from "issued" column
tibble_card <- tibble(card) %>%
  separate(issued, into = c("date", "hour"), sep = " ") %>%
  mutate(date = ymd (date)) %>% select (-hour)

#Client
# • Create column "sex" with male and female genders
# • Standardize date pattern for both genders
# client_id | birth_number | sex | district_id


tibble_client <- tibble(client) %>%
  #Modifying birth number to  chr
  mutate(birth_number = as.character(birth_number)) %>%
  #Is third position is greater than 1 ? woman : man
  mutate(sex = ifelse(substr(birth_number, 3, 3) > 1, 'Female', 'Male')) %>%
  #Setting birth number back to int
  mutate(birth_number = as.integer(birth_number)) %>%
  #Removing 5000 for date if sex = female
  mutate(birth_number = ifelse(sex == 'Female', birth_number - 5000, birth_number))

#Adding 19000000 to convert birth number to date
tibble_client <-
  mutate(tibble_client, birth_number = as.character(birth_number + 19000000))
tibble_client <-
  mutate(tibble_client, birth_number = as.Date(birth_number, '%Y%m%d'))

#Since the last transaction was done in 1998-12-31, we calculate the age of each client
tibble_client <-
  mutate(tibble_client, age = as.integer(as.integer(
    difftime(as.Date('1998-12-31'),
             birth_number, units = 'auto')
  ) / 365))

#Clients distribution per age - the clients are between 20 and 60 years old
hist(tibble_client$age) #FILE - Figure1

#Clients distribution per sex - the clients are well balanced between men and women
tibble_client <-
  mutate(tibble_client, sex = ifelse(sex == 'Female', 0, 1))
hist(tibble_client$sex) #FILE - Figure2
count(tibble_client, vars = sex) #2645 women x 2724 men



#Disposition
tibble_disposition <- tibble(disposition)

#District
# • Attach columns A5:A8 in 
# 1 - <499; 2 - 500 < x < 1999; 3 - 2000 < x < 9999; 4 - >10000
# • Separate columns A12, A13, A15, A16 in "Year", "Unemployment rate" and "Commited Crimes"

tibble_district <- tibble(district) %>%
  mutate_at(vars(`A12`), ~ gsub("[?]", "0", .)) %>%
  mutate_at(vars(`A12`), ~ as.numeric(.)) %>%
  mutate_at(vars(`A15`), ~ gsub("[?]", "0", .)) %>%
  mutate_at(vars(`A15`), ~ as.integer(.))

new_district = tibble("A1" = district$A1,
                      "1995" = district$A15,
                      "1996" = district$A16)
gather(new_district, key = 'year', value = 'commited_crimes', '1995', '1996') -> new_district

district %>% rename("1995" = A12, "1996" = A13) -> district
gather(district, key = 'year', value = 'unemployment_rate', '1995', '1996') -> district

inner_join(district, new_district, by = c("A1" = "A1", "year" = "year")) -> district

district$A15 = NULL
district$A16 = NULL

? rename

district %>% rename(
  "code" = A1,
  "name" = A2,
  "region" = A3,
  "inhabitants" = A4,
  "mun<499" = A5,
  "500<mun<1999" = A6,
  "2000<mun<9999" = A7,
  "mun>10000" = A8,
  "cities" = A9,
  "ratio_urban_inhab" = A10,
  "average_salary" = A11,
  "entrep_per_1000" = A14
) -> district

tibble_district <- tibble(district)
#Loan
tibble_loan <- tibble(loan)

#Order
# • Swap k-symbol for values translation
# "POJISTNE" stands for insurrance
#payment
#"SIPO" stands for household payment
#"LEASING" stands for leasing
#"UVER" stands for loan payment

tibble_order <- tibble(order) %>%
  
  mutate_at(vars(`k_symbol`),
            ~ gsub("POJISTNE", "insurance_payment", `k_symbol`)) %>%
  mutate_at(vars(`k_symbol`),
            ~ gsub("SIPO", "household_payment", `k_symbol`)) %>%
  mutate_at(vars(`k_symbol`), ~ gsub("LEASING", "leasing", `k_symbol`)) %>%
  mutate_at(vars(`k_symbol`), ~ gsub("UVER", "loan_payment", `k_symbol`))

#Transaction
# • Swap k-symbol, type and operation for values translation

# Type:
# "PRIJEM" stands for credit
# "VYDAJ" stands for withdrawal

# Operation:
#"VYBER KARTOU" credit card
#withdrawal
#"VKLAD" credit in cash
#"PREVOD Z UCTU" collection from
#another bank
#"VYBER" withdrawal in cash
#"PREVOD NA UCET" remittance to
#another bank

# K-symbol:
#"POJISTNE" stands for insurance payment
#"SLUZBY" stands for payment for
#statement
#"UROK" stands for interest credited
#"SANKC. UROK" sanction interest if
#negative balance
#"SIPO" stands for household
#"DUCHOD" stands for oldage
#pension
#"UVER" stands for loan payment

tibble_transaction <- tibble(transaction) %>%
  mutate(type = ifelse(
    type == 'PRIJEM',
    'Credit',
    ifelse(type == 'VYDAJ', 'Withdrawal', '')
  )) %>%
  mutate(operation = ifelse(
    operation == 'VYBER KARTOU',
    'Credit card withdrawal',
    ifelse(
      operation == 'VKLAD',
      'Credit in cash',
      ifelse(
        operation == 'PREVOD Z UCTU',
        'Collection from another bank',
        ifelse(
          operation == 'VYBER',
          'Withdrawal in cash',
          'Remmitance to another bank'
        )
      )
    )
  )) %>%
  mutate(k_symbol = ifelse(
    k_symbol == 'POJISTNE',
    'Insurance payment',
    ifelse(
      k_symbol == 'SLUZBY',
      'Payment for statement',
      ifelse(
        k_symbol == 'UROK',
        'Interest credited',
        ifelse(
          k_symbol == 'SANKC. UROK',
          'Sanction interest if negative balance',
          ifelse(
            k_symbol == 'SIPO',
            'Household',
            ifelse(k_symbol == 'DUCHOD', 'Old age pension',
                   'Loan payment')
          )
        )
      )
    )
  ))



#Joining Client, Disposition, Account, Transaction and creating average clients amount and salary per district

client_account <- inner_join(tibble_client, tibble_district,
                             by = c('district_id' = 'A1'))

district_info <- count(client_account, A2, A4, A11) %>%
  mutate(district = A2) %>%
  select(-A2) %>%
  mutate(clients = n) %>%
  select(-n) %>%
  mutate(inhabitants = A4) %>%
  select(-A4) %>%
  mutate(avg_salary = A11) %>%
  select(-A11) %>%
  mutate(client_ratio_percent = round((clients / inhabitants) * 100, 6))

#Country average salary
mean(district_info$avg_salary)
#Country average clients ratio
mean(district_info$client_ratio_percent)

#Top 10 districts with the greatest average salaries
top10_avg_salary <- #FILE - Figure3
  ggplot(data = top_n(district_info, 10, avg_salary),
         aes(x = reorder(district, avg_salary), y = avg_salary)) +
  geom_bar(stat = 'identity') + coord_flip() + labs(x = 'District', y =
                                                      'Avg. Salary')

#Top 10 districts with the greatest number of inhabitants
top10_avg_inhabitants <- #FILE - Figure4
  ggplot(data = top_n(district_info, 10, inhabitants),
         aes(x = reorder(district, inhabitants), y = inhabitants)) +
  geom_bar(stat = 'identity') + coord_flip() + labs(x = 'District', y =
                                                      'Inhabitants')

#Top 10 districts with the greatest client ratio
top10_client_ratio <- #FILE - Figure5
  ggplot(
    data = top_n(district_info, 10, client_ratio_percent),
    aes(x = reorder(district, client_ratio_percent),
        y = client_ratio_percent)
  ) +
  geom_bar(stat = 'identity') + coord_flip() + labs(x = 'District', y =
                                                      'Client ratio (%)')

#Clients distribution per district
client_per_district <-
  ggplot(data = district_info, aes(x = reorder(district, clients), y = clients)) +
  geom_bar(stat = 'identity') + coord_flip() + labs(x = 'District', y =
                                                      'N° of Clients')


#If we have a look at the 3 charts, there are various districts with high average salaries and number of inhabitants, but with no high #client/district ratio. We can then infer that some of these districts can be very attractive for the bank: Hl.m. Praha, Brno - mesto, Ostrava #- mesto and Karvina.


#Creating the definitive dataframe to work on

df_info <-  full_join(tibble_account,
            tibble_disposition,
            by = c('account_id' = 'account_id')) %>%
  #We don't need people that are not in a loan contract
  left_join(tibble_loan, by = c('account_id' = 'account_id')) %>%
  
  full_join(tibble_client, by = c('client_id' = 'client_id')) %>%
  full_join(tibble_district, by = c('district_id.x' = 'code')) %>%
  full_join(tibble_card, by = c('disp_id' = 'disp_id')) %>%
  rename(
    'district_id' = district_id.x,
    'disposition_type' = type.x,
    'card_type' = type.y,
    'card_issued_date' = date,
    'account_date' = date.x,
    'loan_date' = date.y,
    'district' = name,
    'statements_issuance_frequency' = frequency,
    'loan_amount' = amount,
    'loan_duration' = duration,
    'loan_monthly_payments' = payments,
    'loan_status' = status,
    'client_sex' = sex,
    'client_age' = age,
    
    
  ) %>%
  mutate(loan_date = as.character(loan_date + 19000000)) %>%
  mutate(loan_date = as.Date(loan_date, '%Y%m%d')) %>%
  mutate(client_has_card = ifelse(card_type != 'junior' & card_type != 'classic' & card_type != 'gold', 0, 1)) %>%
  #Total per client
  select(-district_id.y) %>%
  #Total per owner clients since only then can ask for loans
  filter(!is.na(loan_id)) %>%
  filter(disposition_type == 'OWNER') %>%
  #Inserting account age - it's possible that the bank may provide more money in a loan for older clients  
  mutate(account_age = as.integer(as.integer(
    difftime(as.Date('1998-12-31'),
             account_date, units = 'auto')
  ) / 365)) %>%
  select(
    -account_id,
    -district_id,
    -disp_id,
    -client_id,
    -loan_id,
    -card_id,
    -district,
    -disposition_type,
    -account_date,
    -birth_number
  )

  
#Looking back at Prague's history, there was a political crisis in 1997 due to financial irregularities from the Civil
#Democratic Party. We can see that this is a huge influence factor for the increasing unemployment rate from 1995 to 1996.

#Unemployment rate 1995 x 1996
ggplot(data=tibble_district) + geom_bar(aes(x=year, y=unemployment_rate), stat='identity') #FILE - Figure6

############### Prediction Model Parameters ###########################################################################
#Predição da quantidade de dinheiro num empréstimo para pessoas que já pediram um empréstimo antes. Dessa forma,
#automatizamos o padrão de valor de empréstimo para um cliente com base no histórico do banco.

#We want to predict how much money the bank provides to a customer that has requested some before. In this way,
#we can automatize the loan amount pattern for a client based on the banks records

#Possible influences:

#Clients:
  # client age, client sex, account age, card type, region
#Loans:
  #  loan duration, loan monthly payments, loan status
#Districts:
  #  average salary, unemployment rate, commited crimes, enterpreneurs, inhabitants


#Checking client's age in the prediction model
lm(data=df_info, formula=loan_amount~client_age) -> model_client_age
summary(model_client_age) 
#Extremely low p-value, is a good influence parameter for the loan amount.

#Checking client's gender in the prediction model
lm(data=df_info, formula=loan_amount~client_sex) -> model_client_sex
summary(model_client_sex)
boxplot(df_info$loan_amount ~ df_info$client_sex) #FILE - Figure7
#p-value = 20%. It means that the client gender is not a good parameter and does not affect the loan amount.

#Checking account's age in the prediction model
lm(data=df_info, formula=loan_amount~account_age) -> model_account_age
summary(model_account_age) 
#P-value < 5%. It means that the account age is a good parameter for the loan amount.

#Checking if the customer having a card affects the prediction model
df_info$client_has_card[is.na(df_info$client_has_card)] <- 0
boxplot(df_info$loan_amount~df_info$client_has_card) #FILE - Figure8
lm(data=df_info, formula=loan_amount~client_has_card) -> model_client_has_card
summary(model_client_has_card)
#p-value = 15%. It means that the client having a card is not a good parameter and does not affect the loan amount.

#Checking loan's duration in the prediction model
hist(df_info$loan_duration) #FILE - Figure9
lm(data=df_info, formula=loan_amount~loan_duration) -> model_loan_duration
summary(model_loan_duration)
#Low p-value, is a good influence parameter for the loan amount.

#Checking loan's monthly payment in the prediction model
lm(data=df_info, formula=loan_amount~loan_monthly_payments) -> model_loan_monthly_payments
ggplot(data=df_info) + geom_smooth(aes(x=loan_monthly_payments, y=loan_amount)) #FILE - Figure10
summary(model_loan_monthly_payments)
#Low p-value, is a good influence parameter for the loan amount. It can mean that the bank loans you more money if your monthly payments are higher.

#Checking loan's status in the prediction model
lm(data=df_info, formula=loan_amount~loan_status) -> model_loan_status
boxplot(df_info$loan_amount~df_info$loan_status) #FILE - Figure11
summary(model_loan_status)
#Low p-value, is a good influence parameter for the loan amount.

#Checking district's average salary in the prediction model
lm(data=df_info, formula=loan_amount~average_salary) -> model_average_salary
hist(df_info$average_salary) #FILE - Figure12
summary(model_average_salary)

#Removing >11000 outliers
df_info_salary_outlier <- filter(df_info, average_salary < 11000 )
lm(data=df_info_salary_outlier, formula=loan_amount~average_salary) -> model_average_salary_outlier
summary(model_average_salary_outlier)
#p-value = 9%, so it's propably not a good influence parameter for the prediction model

#Checking district's unemployment rate in the prediction model
df_info <- mutate(df_info, unemployment_rate = as.numeric(unemployment_rate))
lm(data=df_info, formula=loan_amount~unemployment_rate) -> model_unemployment_rate
summary(model_unemployment_rate)
#p-value = 13%, so it's propably not a good influence parameter for the prediction model

#Checking district's commited crimes in the prediction model
df_info <- mutate(df_info, commited_crimes = as.numeric(commited_crimes))
lm(data=df_info, formula=loan_amount~commited_crimes) -> model_commited_crimes
ggplot(data=df_info) + geom_line(aes(x=commited_crimes, y=loan_amount)) #FILE - Figure13
summary(model_commited_crimes)

#Removing commited crimes outliers above 85000
df_info_commited_crimes_outliers <- filter(df_info, commited_crimes < 85000)
lm(data=df_info_commited_crimes_outliers, formula=loan_amount~commited_crimes) -> model_commited_crimes_outliers
summary(model_commited_crimes_outliers)
#p-value = 43%, so it's propably not a good influence parameter for the prediction model

#Checking district's number of entrepreneurs in the prediction model
lm(data=df_info, formula=loan_amount~entrep_per_1000) -> model_enterpreneurs
summary(model_enterpreneurs)
#p-value = 43%, so it's propably not a good influence parameter for the prediction model

#Checking district's number of inhabitants of entrepreneurs in the prediction model
lm(data=df_info, formula=loan_amount~inhabitants) -> model_inhabitants
summary(model_inhabitants)
#p-value = 60%, so it's propably not a good influence parameter for the prediction model



############### Prediction model ###########################################################################

#How much money the bank should provide in a loan to a client that has already asked for one before?

#High influence parameters: client's age, account's age, loan's duration, loan monthly payments and loan's status
lm(data=df_info, formula = loan_amount~client_age + loan_duration + loan_monthly_payments +
     loan_status) -> predict1
predict (predict1, df_info)

#Adding predictions
add_predictions(data = df_info, model = predict1) -> predmodel1



#We got the following model prediction result in comparison to the bank's records
ggplot(data=predmodel1, show.legend=TRUE) + geom_smooth(aes(x = as.numeric(row.names(predmodel1)), y=pred, color='Prediction')) + 
                                        geom_smooth(aes(x = as.numeric(row.names(predmodel1)), y=loan_amount, color='Loan real amount'))+
  xlab('Data ID') + ylab('Loan Amount') + scale_colour_manual(name='Legend', values=c('red', 'blue')) #FILE - Figure14

#We can check that the previous model showed some high p-values in the account's age and in some of the loan's status. Therefore, we refine these parameters so we can filter and optimize the achieved
#results. So, we can have a model with better precision to predict the loan amount provided to a client that has a loan still active and is in debt with the bank.

summary(predict1)

##Creating dummies to loan status
mutate(df_info, statusB = (loan_status=="B")*1, statusC = (loan_status=="C")*1,
       statusD= (loan_status=="D")*1) -> df_info


#Creating prediction model with clients in an active loan and in debt
lm(data=df_info, formula = loan_amount~client_age + account_age + loan_duration + loan_monthly_payments +
     statusD) -> predict2
summary(predict2)


#This result presented a greater approach for the model. In this way, the bank will be able to know how much money will be available and how much it can provide other clients.

predict(predict2, df_info)

add_predictions(data = df_info, model = predict2) -> predmodel1

#Result of the model
ggplot(data=predmodel1, show.legend=TRUE) + geom_smooth(aes(x = as.numeric(row.names(predmodel1)),
                                                            y=pred, color='Prediction')) + 
  geom_smooth(aes(x = as.numeric(row.names(predmodel1)), y=loan_amount, color='Loan real amount'))+
  xlab('Data ID') + ylab('Loan Amount') + scale_colour_manual(name='Legend', values=c('red', 'blue')) #FILE - Figure15

