#include <iostream>
#include <stack>
#include <vector>
#include <cstdlib>  // For rand() to generate random numbers
#include <ctime>    // For time() to seed the random number generator

using namespace std;

// Partition function using the Lomuto Partition Scheme
// This function selects a pivot (last element) and partitions the array such that
// all elements smaller than or equal to the pivot are on the left,
// and all elements greater than the pivot are on the right.
int partition(vector<int>& arr, int low, int high) {
    int pivot = arr[high]; // Select the last element as the pivot
    int i = low - 1;       // Index for smaller elements

    // Iterate through the array and place elements smaller than or equal to the pivot in the correct position
    for (int j = low; j < high; j++) {
        if (arr[j] <= pivot) {  // If the current element is smaller than or equal to pivot
            i++;                // Move the boundary of smaller elements
            swap(arr[i], arr[j]); // Swap current element with the element at index i
        }
    }
    swap(arr[i + 1], arr[high]); // Place the pivot in its correct position
    return i + 1;  // Return the pivot index
}

// Non-recursive implementation of Quicksort using an explicit stack
void quicksort(vector<int>& arr) {
    stack<pair<int, int>> s; // Stack to store subarrays to be sorted (low, high index pairs)
    s.push({0, arr.size() - 1}); // Push the entire array to the stack

    // Process elements until stack is empty
    while (!s.empty()) {
        auto [low, high] = s.top(); // Extract the topmost range to process
        s.pop(); // Remove it from the stack

        if (low < high) { // Only process if the range has more than one element
            int pivotIndex = partition(arr, low, high); // Partition the array

            // Push left subarray (elements before pivot) onto the stack
            s.push({low, pivotIndex - 1});

            // Push right subarray (elements after pivot) onto the stack
            s.push({pivotIndex + 1, high});
        }
    }
}

int main() {
    srand(time(0));  // Seed the random number generator with current time

    int n = 6;  // Define the number of elements in the array
    vector<int> arr(n);  // Create a vector of size n

    // Populate the array with random numbers between 1 and 100
    for (int i = 0; i < n; i++) {
        arr[i] = rand() % 100 + 1;
    }

    // Print the unsorted array
    cout << "Unsorted array: ";
    for (int num : arr) cout << num << " ";
    cout << endl;

    // Perform quicksort on the array
    quicksort(arr);

    // Print the sorted array
    cout << "Sorted array: ";
    for (int num : arr) cout << num << " ";
    cout << endl;

    return 0;  // Indicate successful program execution
}