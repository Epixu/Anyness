#pragma once
#include <concepts>


namespace Langulus
{

   /// Used as a return type in unsupported functions                         
   struct Unsupported {};

} // namespace Langulus

namespace Langulus::CT
{

   /// Check if any T is the built-in one that signifies lack of support      
   template<class...T>
   concept Unsupported = (::std::same_as<::Langulus::Unsupported, T> or ...);

   /// Check if all T are supported                                           
   template<class...T>
   concept Supported = ((not Unsupported<T>) and ...);

} // namespace Langulus::CT

