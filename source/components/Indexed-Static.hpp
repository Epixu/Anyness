#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Integer.hpp>


namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::Index<T>:                       
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Index = Yes;` in T                         
   template<class T>
   struct Index {
      static constexpr bool Enabled = CT::Integer<T>;
   };

} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(Index);

namespace Langulus::Anyness::Index
{
   namespace Inner
   {
      struct All      { using CTTI_Index = Yes; };
      struct Many     { using CTTI_Index = Yes; };
      struct Single   { using CTTI_Index = Yes; };
      struct None     { using CTTI_Index = Yes; };
      struct Front    { using CTTI_Index = Yes; };
      struct Middle   { using CTTI_Index = Yes; };
      struct Back     { using CTTI_Index = Yes; };
      struct Mode     { using CTTI_Index = Yes; };
      struct Biggest  { using CTTI_Index = Yes; };
      struct Smallest { using CTTI_Index = Yes; };
      struct Random   { using CTTI_Index = Yes; };
      struct First    { using CTTI_Index = Yes; };
      struct Last     { using CTTI_Index = Yes; };

   } // namespace Langulus::Anyness::Index::Inner

   /// Equivalent to the container's initialized count                        
   constexpr auto All = Inner::All {};

   /// Equivalent to a count larger than one                                  
   constexpr auto Many = Inner::Many {};

   /// Equivalent to a count of one                                           
   constexpr auto Single = Inner::Single {};

   /// Equivalent to a count of zero, or Container::CountType's max value     
   /// For std containers it's equivalent to 'npos'                           
   constexpr auto None = Inner::None {};

   /// Equivalent to the space before the first element in continuous memory  
   constexpr auto Front = Inner::Front {};

   /// Equivalent to the middle element of continuous memory, or the space    
   /// between the two middle elements, in the case that count is even        
   constexpr auto Middle = Inner::Middle {};

   /// Equivalent to the space after the last element in continuous memory    
   /// i.e. one-past the last element                                         
   constexpr auto Back = Inner::Back {};

   /// Equivalent to the first index of the most occuring element in memory   
   constexpr auto Mode = Inner::Mode {};

   /// Equivalent to the index of the first largest element in memory         
   constexpr auto Biggest = Inner::Biggest {};

   /// Equivalent to the index of the first smallest element in memory        
   constexpr auto Smallest = Inner::Smallest {};

   /// A random index inside a valid memory range                             
   constexpr auto Random = Inner::Random {};

   /// Equivalent to 0 for continuous memory                                  
   constexpr auto First = Inner::First {};

   /// Equivalent to size-1 for continuous memory                             
   constexpr auto Last = Inner::Last {};

} // namespace Langulus::Anyness::Index

namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Provides random element access based on a linear index, that is        
   /// mapped directly onto continuous memory                                 
   ///   @tparam T - constrain the type of allowed indices. Leave as 'void'   
   ///      to allow for all the usual integer types. A map would use the     
   ///      'key' type instead                                                
   ///                                                                        
   template<class T = void>
   struct IndexedStatic {
      using CTTI_Component = Yes;
      static constexpr bool Indexed = true;
   };

} // namespace Langulus::Anyness::Component
