CREATE SCHEMA `darsh_project` ;

-- Create tables section -------------------------------------------------


CREATE TABLE `Customer`
(
  `Cust_id` Int UNSIGNED NOT NULL AUTO_INCREMENT,
  `Cust_fname` Varchar(40) NOT NULL,
  `Cust_lname` Varchar(40) NOT NULL,
  `Cust_email` Varchar(40) NOT NULL,
  PRIMARY KEY (`Cust_id`)
)
;

ALTER TABLE `Customer` ADD UNIQUE `Cust_email` (`Cust_email`)
;

CREATE TABLE `Cart`
(
  `Cust_id` Int UNSIGNED NOT NULL,
  `Cart_id` Int NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (`Cart_id`)
)
;

CREATE INDEX `IX_Relationship2` ON `Cart` (`Cust_id`)
;

-- Table Customer_address

CREATE TABLE `Customer_address`
(
  `Address_id` Int NOT NULL AUTO_INCREMENT,
  `Street` Varchar(20) NOT NULL,
  `City` Varchar(20) NOT NULL,
  `State` Varchar(20) NOT NULL,
  `Cust_id` Int UNSIGNED,
  PRIMARY KEY (`Address_id`)
)
;

CREATE INDEX `IX_Has` ON `Customer_address` (`Cust_id`)
;

-- Table Feedback

CREATE TABLE `Feedback`
(
  `Rating` Tinyint NOT NULL,
  `Comment` Varchar(500),
  `Date` Date,
  `Cust_id` Int UNSIGNED,
  `Feedback_id` Int NOT NULL AUTO_INCREMENT,
  `Product_id` int,
  PRIMARY KEY (`Feedback_id`)
)
;

CREATE INDEX `IX_Relationship4` ON `Feedback` (`Product_id`)
;

CREATE INDEX `IX_gives` ON `Feedback` (`Cust_id`)
;

-- Table Product

CREATE TABLE `Product`
(
  `Product_id` Int NOT NULL AUTO_INCREMENT,
  `Product_name` Varchar(200) NOT NULL,
  `Price` Varchar(15) NOT NULL,
  `Alley_number` Varchar(20),
  `Stack_number` Varchar(20),
  `Quantity` Int NOT NULL,
  PRIMARY KEY (`Product_id`)
)
;

ALTER TABLE `Product` ADD UNIQUE `Product_name` (`Product_name`)
;

-- Table Customer_phone

CREATE TABLE `Customer_phone`
(
  `Cust_id` Int UNSIGNED,
  `Phone_number` Int NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (`Phone_number`)
)
;

CREATE INDEX `IX_has` ON `Customer_phone` (`Cust_id`)
;

-- Table CartProductLink


CREATE TABLE `CartProductLink`
(
  `Cart_product_link_id` Int NOT NULL AUTO_INCREMENT,
  `Cart_id` Int,
  `Product_id` Int,
  PRIMARY KEY (`Cart_product_link_id`)
)
;

CREATE INDEX `IX_R11` ON `CartProductLink` (`Product_id`)
;

CREATE INDEX `IX_Contains` ON `CartProductLink` (`Cart_id`)
;

-- Table Warehouse

CREATE TABLE `Warehouse`
(
  `City` Varchar(20) NOT NULL,
  `State` Varchar(20) NOT NULL,
  `Available_space` Varchar(20),
  `Net_worth` Varchar(20) NOT NULL,
  `Warehouse_id` Int NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (`Warehouse_id`)
)
;

-- Table SKU
CREATE TABLE `SKU`
(
  `SKU` Int NOT NULL AUTO_INCREMENT,
  `Product_id` Int,
  `Warehouse_id` Int,
  PRIMARY KEY (`SKU`)
)
;

CREATE INDEX `IX_r12` ON `SKU` (`Warehouse_id`)
;

CREATE INDEX `IX_r13` ON `SKU` (`Product_id`)
;

-- Table Vendor

CREATE TABLE `Vendor`
(
  `Vendor_name` Varchar(20) NOT NULL,
  `Vendor_phone` Varchar(20) NOT NULL,
  `Vendor_id` Int NOT NULL AUTO_INCREMENT,
  `Warehouse_id` Int,
  PRIMARY KEY (`Vendor_id`)
)
;

CREATE INDEX `IX_Relationship10` ON `Vendor` (`Warehouse_id`)
;

-- Table Vendor_address

CREATE TABLE `Vendor_address`
(
  `Vendor_address_id` Int NOT NULL AUTO_INCREMENT,
  `Street` Varchar(30) NOT NULL,
  `City` Varchar(20) NOT NULL,
  `State` Varchar(20) NOT NULL,
  `Vendor_id` Int,
  PRIMARY KEY (`Vendor_address_id`)
)
;

CREATE INDEX `IX_R10` ON `Vendor_address` (`Vendor_id`)
;

-- Table Invoice

CREATE TABLE `Invoice`
(
  `Cart_id` Int,
  `Payment_id` Int,
  `Invoice_id` Int NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (`Invoice_id`)
)
;

CREATE INDEX `IX_R22` ON `Invoice` (`Payment_id`)
;

CREATE INDEX `IX_Relationship14` ON `Invoice` (`Cart_id`)
;

-- Table Payment_details

CREATE TABLE `Payment_details`
(
  `Payment_id` Int NOT NULL AUTO_INCREMENT,
  `Cust_id` Int UNSIGNED,
  PRIMARY KEY (`Payment_id`)
)
;


CREATE INDEX `IX_Relationship16` ON `Payment_details` (`Cust_id`)
;


-- Table Card

CREATE TABLE `Card`
(
  `Card_id` Int NOT NULL AUTO_INCREMENT,
  `Name` Varchar(20) NOT NULL,
  `Number` Varchar(20) NOT NULL,
  `cvv` Char(3) NOT NULL,
  `Payment_id` Int NOT NULL,
  PRIMARY KEY (`Card_id`)
)
;

CREATE INDEX `IX_Relationship21` ON `Card` (`Payment_id`)
;

CREATE TABLE `Vendor_bank_details`
(
  `Vendor_bank_id` Int NOT NULL AUTO_INCREMENT,
  `Bank_name` Varchar(20) NOT NULL,
  `Account_number` Int UNSIGNED NOT NULL,
  `Routing_number` Int NOT NULL,
  `Vendor_id` Int NOT NULL,
  PRIMARY KEY (`Vendor_bank_id`)
)
;
CREATE INDEX `IX_r15` ON `Vendor_bank_details` (`Vendor_id`)
;
-- drop table vendor_payment;

CREATE TABLE `Vendor_payment`
(
  `Vendor_payment_id` Int NOT NULL AUTO_INCREMENT,
  `Vendor_bank_id` Int NOT NULL,
  PRIMARY KEY (`Vendor_payment_id`)
)
;

CREATE INDEX `IX_r24` ON `Vendor_payment` (`Vendor_bank_id`)
;

CREATE TABLE product_log
( 
  `Log_id` INT NOT NULL auto_increment,
  `Product_id` Int NOT NULL,
  `Product_name` Varchar(200) NOT NULL,
  `Quantity` Int NOT NULL,
  PRIMARY KEY (`Log_id`)
);
  
	
CREATE TABLE product_log_vendor
( 
  `Log_id_vendor` INT NOT NULL auto_increment,
  `Product_id` Int NOT NULL,
  `Product_name` Varchar(200) NOT NULL,
  `Quantity` Int NOT NULL,
  `Vendor_id` INT NOT NULL,
  `Warehouse_id` INT NOT NULL,
  PRIMARY KEY (`Log_id_vendor`)
);
-- Create relationships section ------------------------------------------------- 

ALTER TABLE `Customer_address` ADD CONSTRAINT `Has` FOREIGN KEY (`Cust_id`) REFERENCES `Customer` (`Cust_id`) ON DELETE RESTRICT ON UPDATE RESTRICT
;

ALTER TABLE `Feedback` ADD CONSTRAINT `gives` FOREIGN KEY (`Cust_id`) REFERENCES `Customer` (`Cust_id`) ON DELETE RESTRICT ON UPDATE RESTRICT
;

ALTER TABLE `Feedback` ADD CONSTRAINT `Relationship4` FOREIGN KEY (`Product_id`) REFERENCES `Product` (`Product_id`) ON DELETE RESTRICT ON UPDATE RESTRICT
;

ALTER TABLE `Customer_phone` ADD CONSTRAINT `relationship19` FOREIGN KEY (`Cust_id`) REFERENCES `Customer` (`Cust_id`) ON DELETE RESTRICT ON UPDATE RESTRICT
;

ALTER TABLE `CartProductLink` ADD CONSTRAINT `IX_R11` FOREIGN KEY (`Product_id`) REFERENCES `Product` (`Product_id`) ON DELETE RESTRICT ON UPDATE RESTRICT
;

ALTER TABLE `CartProductLink` ADD CONSTRAINT `Contains` FOREIGN KEY (`Cart_id`) REFERENCES `Cart` (`Cart_id`) ON DELETE RESTRICT ON UPDATE RESTRICT
;

ALTER TABLE `SKU` ADD CONSTRAINT `IX_r13` FOREIGN KEY (`Product_id`) REFERENCES `Product` (`Product_id`) ON DELETE RESTRICT ON UPDATE RESTRICT
;

ALTER TABLE `SKU` ADD CONSTRAINT `IX_r12` FOREIGN KEY (`Warehouse_id`) REFERENCES `Warehouse` (`Warehouse_id`) ON DELETE RESTRICT ON UPDATE RESTRICT
;

ALTER TABLE `Vendor` ADD CONSTRAINT `delivers` FOREIGN KEY (`Warehouse_id`) REFERENCES `Warehouse` (`Warehouse_id`) ON DELETE RESTRICT ON UPDATE RESTRICT
;

ALTER TABLE `Vendor_address` ADD CONSTRAINT `IX_R10` FOREIGN KEY (`Vendor_id`) REFERENCES `Vendor` (`Vendor_id`)
;

ALTER TABLE `Invoice` ADD CONSTRAINT `Pays` FOREIGN KEY (`Cart_id`) REFERENCES `Cart` (`Cart_id`) ON DELETE RESTRICT ON UPDATE RESTRICT
;

ALTER TABLE `Invoice` ADD CONSTRAINT `IX_R22` FOREIGN KEY (`Payment_id`) REFERENCES `payment_details` (`payment_id`) ON DELETE RESTRICT ON UPDATE RESTRICT
;

ALTER TABLE `Payment_details` ADD CONSTRAINT `Relationship16` FOREIGN KEY (`Cust_id`) REFERENCES `Customer` (`Cust_id`) ON DELETE RESTRICT ON UPDATE RESTRICT
;

ALTER TABLE `Card` ADD CONSTRAINT `Relationship21` FOREIGN KEY (`Payment_id`) REFERENCES `Payment_details` (`Payment_id`) ON DELETE RESTRICT ON UPDATE RESTRICT
;

ALTER TABLE `Vendor_bank_details` ADD CONSTRAINT `IX_r15` FOREIGN KEY (`Vendor_id`) REFERENCES `Vendor` (`Vendor_id`) ON DELETE RESTRICT ON UPDATE RESTRICT
;

ALTER TABLE `cart` ADD CONSTRAINT `IX_Relationship2` FOREIGN KEY (`cust_id`) REFERENCES `customer` (`cust_id`) ON DELETE RESTRICT ON UPDATE RESTRICT
;

ALTER TABLE `vendor_payment` ADD CONSTRAINT `IX_r24` FOREIGN KEY (`Vendor_bank_id`) REFERENCES `vendor_bank_details` (`Vendor_bank_id`) ON DELETE RESTRICT ON UPDATE RESTRICT
;



-- Populating tables--

INSERT INTO customer (cust_fname, cust_lname, cust_email) VALUES ('Jon','Snow','Jonsnow@gmail.com');
INSERT INTO customer (cust_fname, cust_lname, cust_email) VALUES ('Black','Jack','Blackjack@gmail.com');
INSERT INTO customer (cust_fname, cust_lname, cust_email) VALUES ('Fae','Raegan','Faeregan@gmail.com');
INSERT INTO customer (cust_fname, cust_lname, cust_email) VALUES ('Megan','Fawaz','Meganfawaz@gmail.com');
INSERT INTO customer (cust_fname, cust_lname, cust_email) VALUES ('Brad','Fawaz','Bradfawaz@gmail.com');

INSERT INTO customer_address (street ,city, state, cust_id) VALUES ('State Street','Boston','MA', 1);
INSERT INTO customer_address (street ,city, state, cust_id) VALUES ('Old Avenue','Boston','MA', 2);
INSERT INTO customer_address (street ,city, state, cust_id) VALUES ('State Street','New York','NY', 3);
INSERT INTO customer_address (street ,city, state, cust_id) VALUES ('Old Avenue','Boston','MA', 4);
INSERT INTO customer_address (street ,city, state, cust_id) VALUES ('Old Avenue','Boston','MA', 5);


INSERT INTO customer_phone (cust_id, phone_number) VALUES (1,'123456789');
INSERT INTO customer_phone (cust_id, phone_number) VALUES (2,'234567890');
INSERT INTO customer_phone (cust_id, phone_number) VALUES (3,'345678901');
INSERT INTO customer_phone (cust_id, phone_number) VALUES (4,'456789012');
INSERT INTO customer_phone (cust_id, phone_number) VALUES (5,'567890123');

INSERT INTO product (Product_name, Price, Alley_number, Stack_number, Quantity) VALUES ('Toy','100', '1','1','100');
INSERT INTO product (Product_name, Price, Alley_number, Stack_number, Quantity) VALUES ('Book','200', '1','1','100');
INSERT INTO product (Product_name, Price, Alley_number, Stack_number, Quantity) VALUES ('Shirt','20', '2','1','100');
INSERT INTO product (Product_name, Price, Alley_number, Stack_number, Quantity) VALUES ('Fruit','2', '2','2','100');
INSERT INTO product (Product_name, Price, Alley_number, Stack_number, Quantity) VALUES ('Jack Fruit','2', '2','3','100');

INSERT INTO feedback (rating, Comment, Date, cust_id, product_id) VALUES ('4','Good', sysdate(),'3','2');
INSERT INTO feedback (rating, Comment, Date, cust_id, product_id) VALUES ('2','Bad', sysdate(),'2','2');
INSERT INTO feedback (rating, Comment, Date, cust_id, product_id) VALUES ('5','Great !', sysdate(),'2','3');
INSERT INTO feedback (rating, Comment, Date, cust_id, product_id) VALUES ('1','Very bad', sysdate(),'4','5');
INSERT INTO feedback (rating, Comment, Date, cust_id, product_id) VALUES ('1','Very bad', sysdate(),'1','5');
INSERT INTO feedback (rating, Comment, Date, cust_id, product_id) VALUES ('1','Very bad', sysdate(),'5','5');
INSERT INTO feedback (rating, Comment, Date, cust_id, product_id) VALUES ('5','Great !', sysdate(),'1','3');
INSERT INTO feedback (rating, Comment, Date, cust_id, product_id) VALUES ('4','', sysdate(),'3','4');
INSERT INTO feedback (rating, Date, cust_id, product_id) VALUES ('4', sysdate(),'3','1');

INSERT INTO warehouse (city, state, available_space, net_worth) VALUES ('Boston','MA','50000','150000');
INSERT INTO warehouse (city, state, available_space, net_worth) VALUES ('New York','NY','500','25000');

INSERT INTO vendor (vendor_name, vendor_phone, warehouse_id) VALUES ('Grocerymart','123456789',1);
INSERT INTO vendor (vendor_name, vendor_phone, warehouse_id) VALUES ('Apparelmart','987654321',2);
INSERT INTO vendor (vendor_name, vendor_phone, warehouse_id) VALUES ('Totalmart','1111111111',1);

INSERT INTO vendor_address (street, city, state, vendor_id) VALUES ('Backway','Boston','MA',1);
INSERT INTO vendor_address (street, city, state, vendor_id) VALUES ('Backway','New York','NY',2);
INSERT INTO vendor_address (street, city, state, vendor_id) VALUES ('Frontway','Boston','MA',3);

INSERT INTO vendor_bank_details (Bank_name, account_number, routing_number, vendor_id) VALUES ('Citi','11111','22222',1);
INSERT INTO vendor_bank_details (Bank_name, account_number, routing_number, vendor_id) VALUES ('Citi','33333','22222',2);
INSERT INTO vendor_bank_details (Bank_name, account_number, routing_number, vendor_id) VALUES ('Chase','44444','55555',3);

INSERT INTO sku (product_id, warehouse_id) VALUES (1,1);
INSERT INTO sku (product_id, warehouse_id) VALUES (2,1);
INSERT INTO sku (product_id, warehouse_id) VALUES (3,1);
INSERT INTO sku (product_id, warehouse_id) VALUES (4,1);
INSERT INTO sku (product_id, warehouse_id) VALUES (5,1);
INSERT INTO sku (product_id, warehouse_id) VALUES (1,2);
INSERT INTO sku (product_id, warehouse_id) VALUES (2,2);
INSERT INTO sku (product_id, warehouse_id) VALUES (3,2);

/* Triggers and transaction for purchase */
-- drop trigger Product_qty;

DELIMITER //

CREATE TRIGGER Product_qty
AFTER INSERT ON cartproductlink
FOR EACH ROW
	BEGIN
    UPDATE product SET product.Quantity = product.Quantity - 1 
		where product_id = (SELECT cartproductlink.Product_id from cartproductlink 
			where cartproductlink.Cart_product_link_id = (Select max(cartproductlink.Cart_product_link_id) from cartproductlink)) ;
    END;//
    
    
DELIMITER ??

CREATE TRIGGER Product_log
AFTER UPDATE ON product
FOR each row
	BEGIN
    INSERT INTO product_log (product_id, Product_name, Quantity) 
    SELECT product_id, Product_name, Quantity FROM Product 
		where product_id = (SELECT cartproductlink.Product_id from cartproductlink 
			where cartproductlink.Cart_product_link_id = (Select max(cartproductlink.Cart_product_link_id) from cartproductlink));
    END;??

/* Transaction */
START TRANSACTION;
INSERT INTO cart (cust_id) VALUES (5);
INSERT INTO payment_details (cust_id) VALUES (5);
SET @var1 := (SELECT max(payment_id) from payment_details);
SET @var2 := (SELECT max(cart_id) from cart);
INSERT INTO cartproductlink (cart_id, product_id) VALUES (@var2,5);
INSERT INTO card (name, number, cvv, payment_id) VALUES ('hgjjjjk','99334991','996', @var1);
INSERT INTO invoice (cart_id, payment_id) SELECT cart.cart_id, payment_details.payment_id from cart 
	INNER JOIN payment_details on cart.cust_id = payment_details.cust_id 
			where cart.cart_id = (Select max(cart.cart_id) from cart);
COMMIT;

/* restock transaction */

SET @quant = 25;
SET @pid = 1;
SET @pname = 'Toy';
SET @wid = 1;
SET @vid = 1;

DELIMITER <>
-- drop trigger restock;
CREATE TRIGGER restock
AFTER INSERT ON product_log_vendor
FOR EACH ROW
BEGIN 
	UPDATE product SET Quantity = Quantity + @quant 
		where Product_id = (Select Product_id from product_log_vendor 
			where Log_id_vendor = (Select max(Log_id_vendor) from product_log_vendor)) ;
END; <>

START TRANSACTION;
	INSERT INTO product_log_vendor (Product_id, Product_name, Quantity, Vendor_id, Warehouse_id) VALUES (@pid,@pname,@quant,@vid,@wid);
	INSERT INTO vendor_payment (vendor_bank_id) SELECT Vendor_bank_id from vendor_bank_details where vendor_id = @vid ;
COMMIT;


/* USER CREATION */

CREATE USER 'Jon'@'localhost' IDENTIFIED BY 'jonpass';
CREATE USER 'Black'@'localhost' IDENTIFIED BY 'blackpass';
CREATE USER 'Fae'@'localhost' IDENTIFIED BY 'faepass';
CREATE USER 'Megan'@'localhost' IDENTIFIED BY 'meganpass';
CREATE USER 'Brad'@'localhost' IDENTIFIED BY 'bradpass';

/*
DROP USER 'Jon'@'localhost' ;
drop USER 'Black'@'localhost' ;
drop USER 'Fae'@'localhost' ;
drop USER 'Megan'@'localhost' ;
drop USER 'Brad'@'localhost' ;
*/
CREATE USER 'Grocerymart'@'localhost' IDENTIFIED BY 'grocerymartpass';
CREATE USER 'Apparelmart'@'localhost' IDENTIFIED BY 'apparelmartpass';
CREATE USER 'Totalmart'@'localhost' IDENTIFIED BY 'totalmartpass';

CREATE USER 'empoloyee_1'@'localhost' IDENTIFIED BY 'employee_1pass';
CREATE USER 'empoloyee_2'@'localhost' IDENTIFIED BY 'employee_2pass';

CREATE USER 'manager'@'localhost' IDENTIFIED BY 'managerpass';

GRANT UPDATE ON customer to 'Jon'@'localhost' IDENTIFIED BY 'jonpass' ;
GRANT UPDATE ON customer to 'Black'@'localhost' IDENTIFIED BY 'blackpass';
GRANT UPDATE ON customer to 'Fae'@'localhost' IDENTIFIED BY 'faepass';
GRANT UPDATE ON customer to 'Megan'@'localhost' IDENTIFIED BY 'meganpass';
GRANT UPDATE ON customer to 'Brad'@'localhost' IDENTIFIED BY 'bradpass';

GRANT UPDATE ON customer_address to 'Jon'@'localhost' IDENTIFIED BY 'jonpass';
GRANT UPDATE ON customer_address to 'Black'@'localhost' IDENTIFIED BY 'blackpass';
GRANT UPDATE ON customer_address to 'Fae'@'localhost' IDENTIFIED BY 'faepass';
GRANT UPDATE ON customer_address to 'Megan'@'localhost' IDENTIFIED BY 'meganpass';
GRANT UPDATE ON customer_address to 'Brad'@'localhost' IDENTIFIED BY 'bradpass';

GRANT UPDATE ON customer_phone to 'Jon'@'localhost' IDENTIFIED BY 'jonpass';
GRANT UPDATE ON customer_phone to 'Black'@'localhost' IDENTIFIED BY 'blackpass';
GRANT UPDATE ON customer_phone to 'Fae'@'localhost' IDENTIFIED BY 'faepass';
GRANT UPDATE ON customer_phone to 'Megan'@'localhost' IDENTIFIED BY 'meganpass';
GRANT UPDATE ON customer_phone to 'Brad'@'localhost' IDENTIFIED BY 'bradpass';

GRANT INSERT ON customer to 'Jon'@'localhost' IDENTIFIED BY 'jonpass';
GRANT INSERT ON customer to 'Black'@'localhost' IDENTIFIED BY 'blackpass';
GRANT INSERT ON customer to 'Fae'@'localhost' IDENTIFIED BY 'faepass';
GRANT INSERT ON customer to 'Megan'@'localhost' IDENTIFIED BY 'meganpass';
GRANT INSERT ON customer to 'Brad'@'localhost' IDENTIFIED BY 'bradpass';

GRANT INSERT ON customer_address to 'Jon'@'localhost' IDENTIFIED BY 'jonpass';
GRANT INSERT ON customer_address to 'Black'@'localhost' IDENTIFIED BY 'blackpass';
GRANT INSERT ON customer_address to 'Fae'@'localhost' IDENTIFIED BY 'faepass';
GRANT INSERT ON customer_address to 'Megan'@'localhost' IDENTIFIED BY 'meganpass';
GRANT INSERT ON customer_address to 'Brad'@'localhost' IDENTIFIED BY 'bradpass';

GRANT INSERT ON customer_phone to 'Jon'@'localhost' IDENTIFIED BY 'jonpass';
GRANT INSERT ON customer_phone to 'Black'@'localhost' IDENTIFIED BY 'blackpass';
GRANT INSERT ON customer_phone to 'Fae'@'localhost' IDENTIFIED BY 'faepass';
GRANT INSERT ON customer_phone to 'Megan'@'localhost' IDENTIFIED BY 'meganpass';
GRANT INSERT ON customer_phone to 'Brad'@'localhost' IDENTIFIED BY 'bradpass';

GRANT INSERT ON feedback to 'Jon'@'localhost' IDENTIFIED BY 'jonpass';
GRANT INSERT ON feedback to 'Black'@'localhost' IDENTIFIED BY 'blackpass';
GRANT INSERT ON feedback to 'Fae'@'localhost' IDENTIFIED BY 'faepass';
GRANT INSERT ON feedback to 'Megan'@'localhost' IDENTIFIED BY 'meganpass';
GRANT INSERT ON feedback to 'Brad'@'localhost' IDENTIFIED BY 'bradpass';

GRANT SELECT ON feedback to 'Jon'@'localhost' IDENTIFIED BY 'jonpass';
GRANT SELECT ON feedback to 'Black'@'localhost' IDENTIFIED BY 'blackpass';
GRANT SELECT ON feedback to 'Fae'@'localhost' IDENTIFIED BY 'faepass';
GRANT SELECT ON feedback to 'Megan'@'localhost' IDENTIFIED BY 'meganpass';
GRANT SELECT ON feedback to 'Brad'@'localhost' IDENTIFIED BY 'bradpass';

GRANT INSERT ON card to 'Jon'@'localhost' IDENTIFIED BY 'jonpass';
GRANT INSERT ON card to 'Black'@'localhost' IDENTIFIED BY 'blackpass';
GRANT INSERT ON card to 'Fae'@'localhost' IDENTIFIED BY 'faepass';
GRANT INSERT ON card to 'Megan'@'localhost' IDENTIFIED BY 'meganpass';
GRANT INSERT ON card to 'Brad'@'localhost' IDENTIFIED BY 'bradpass';

GRANT INSERT ON cart to 'Jon'@'localhost' IDENTIFIED BY 'jonpass';
GRANT INSERT ON cart to 'Black'@'localhost' IDENTIFIED BY 'blackpass';
GRANT INSERT ON cart to 'Fae'@'localhost' IDENTIFIED BY 'faepass';
GRANT INSERT ON cart to 'Megan'@'localhost' IDENTIFIED BY 'meganpass';
GRANT INSERT ON cart to 'Brad'@'localhost' IDENTIFIED BY 'bradpass';

GRANT INSERT ON vendor_bank_details TO  'Grocerymart'@'localhost' IDENTIFIED BY 'grocerymartpass';
GRANT INSERT ON vendor_bank_details TO  'Apparelmart'@'localhost' IDENTIFIED BY 'apparelmartpass';
GRANT INSERT ON vendor_bank_details TO  'Totalmart'@'localhost' IDENTIFIED BY 'totalmartpass';

GRANT INSERT ON vendor_address TO  'Grocerymart'@'localhost' IDENTIFIED BY 'grocerymartpass';
GRANT INSERT ON vendor_address TO  'Apparelmart'@'localhost' IDENTIFIED BY 'apparelmartpass';
GRANT INSERT ON vendor_address TO  'Totalmart'@'localhost' IDENTIFIED BY 'totalmartpass';

GRANT UPDATE ON vendor_bank_details TO  'Grocerymart'@'localhost' IDENTIFIED BY 'grocerymartpass';
GRANT UPDATE ON vendor_bank_details TO  'Apparelmart'@'localhost' IDENTIFIED BY 'apparelmartpass';
GRANT UPDATE ON vendor_bank_details TO  'Totalmart'@'localhost' IDENTIFIED BY 'totalmartpass';

GRANT UPDATE ON vendor_address TO  'Grocerymart'@'localhost' IDENTIFIED BY 'grocerymartpass';
GRANT UPDATE ON vendor_address TO  'Apparelmart'@'localhost' IDENTIFIED BY 'apparelmartpass';
GRANT UPDATE ON vendor_address TO  'Totalmart'@'localhost' IDENTIFIED BY 'totalmartpass';

GRANT SELECT ON product_log TO  'Grocerymart'@'localhost' IDENTIFIED BY 'grocerymartpass';
GRANT SELECT ON product_log TO  'Apparelmart'@'localhost' IDENTIFIED BY 'apparelmartpass';
GRANT SELECT ON product_log TO  'Totalmart'@'localhost' IDENTIFIED BY 'totalmartpass';

GRANT ALL on *.* to 'empoloyee_1'@'localhost' IDENTIFIED BY 'employee_1pass';
GRANT ALL on *.* to 'empoloyee_2'@'localhost' IDENTIFIED BY 'employee_2pass';

GRANT ALL on *.* to 'manager'@'localhost' IDENTIFIED BY 'managerpass';
GRANT GRANT OPTION on *.* to 'manager'@'localhost' IDENTIFIED BY 'managerpass';


-- drop table product_log_vendor;

-- VIEWS

-- Which product is purchased by which customer
SET @cust = 5;
SELECT product_id, product_name from product 
	where product_id IN (SELECT product_id from cartproductlink 
		where cart_id IN (Select cart_id from cart where cust_id = @cust));



-- SELECT user from mysql.user;
   
-- SHOW GRANTS for 'manager'@'localhost';