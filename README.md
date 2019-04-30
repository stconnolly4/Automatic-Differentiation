# Automatic-Differentiation

First checkpoint:

So far, we have read various articles, blog posts, and Wikipedia pages regarding Forward Automatic Differentiation. We created a GitHub repository so that we could better collaborate on this project. We created various data structures and one type (for the environment), and began to create the recursive differentiation function. Our next step is to create a testing infrastructure and various tests to see if our code is working as expected. After completing this step, we plan to finish implementing the differentiation function for the data types we have already defined. Afterwards, we plan to expand the data types and the differentiation function to include more complex functions.


Second checkpoint:

We completed the Forward Mode implementation, created a testing infrastructure, and created multiple tests. Our Forward Mode passed all of our test cases. We then implemented LetE, created a test case for it, and passed the test case. We are happy with our Forward Mode, and we decided to attempt Reverse Mode. We spent a significant amount of time reading about Reverse Mode online, as well as  meeting with Dr. Darais to discuss Reverse Mode. We started to implement Reverse Mode. In our Expr data type, we started with DoubleE and SinE. We brainstormed ways to create the differentiate function, perhaps including another parameter ('derivative'), but we are having trouble implementing it. Therefore, our goal for the rest of the week is to successfully implement SinE in Reverse Mode, but we are unsure if we will be able to complete this goal.