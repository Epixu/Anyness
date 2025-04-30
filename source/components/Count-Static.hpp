#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Index.hpp>


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// A compile-time count                                                   
   /// Count shows how many elements inside a container are initialized       
   /// Compile-time counting isn't really counting, and doesn't take up       
   /// space, but is useful for defining single-element containers, that      
   /// still need the GetCount() API to function alongside other components   
   ///                                                                        
   template<auto COUNT>
   struct CountStatic {
      static_assert(COUNT > 0, "Can't have a container of zero or negative count");

      using CTTI_Component = Yes;
      using CountType = decltype(COUNT);
      using IndexType = Index::At<CountType>;

      /// Always returns false                                                
      constexpr bool IsEmpty() const noexcept { return false; }

      /// Get the compile-time count                                          
      constexpr auto GetCount() const noexcept { return COUNT; }

      /// Having a compile-time count also implies a compile-time capacity    
      constexpr auto GetCapacity() const noexcept { return COUNT; }

      /// Explicit boolean conversion to allow using containers in ifs        
      /// In this case, it always returns true                                
      constexpr explicit operator bool() const noexcept { return true; }
   };

} // namespace Langulus::Anyness::Component
