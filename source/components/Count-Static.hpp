#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Index.hpp>


namespace Langulus::Anyness::Component
{

   ///                                                                        
   ///   A compile-time count                                                 
   ///                                                                        
   ///   Count shows how many elements inside a container are initialized     
   ///   Compile-time counting isn't really counting, and doesn't take up     
   /// space, but is useful for defining single-element containers, that      
   /// still need the API required to function alongside other components.    
   ///   In these cases, count is equal to COUNT if container has a heap      
   /// component that has been allocated. If no heap component exists, then   
   /// the count is simply always COUNT.                                      
   ///                                                                        
   template<auto COUNT>
   struct CountStatic {
      static_assert(COUNT > 0, "Can't have a container of zero or negative count");

      using CTTI_Component = Yes;
      using CountType = decltype(COUNT);
      using IndexType = Index::At<CountType>;

      /// Equal to COUNT if container has a heap component that has been      
      /// allocated. If no heap component exists, then the count is simply    
      /// always COUNT                                                        
      template<CT::Container C>
      constexpr auto GetCount(this C const& self) noexcept -> CountType {
         if constexpr (CT::HeapAllocated<C>) {
            if constexpr (C::HeapCanBeNull)
               return self.GetRaw() ? COUNT : CountType {};
            else
               return COUNT;
         }
         else return COUNT;
      }
      
      /// Always returns false                                                
      template<CT::Container C>
      constexpr bool IsEmpty(this C const& self) noexcept {
         return self.GetCount() == CountType {};
      }

      /// Explicit boolean conversion to allow using containers in ifs        
      template<CT::Container C>
      constexpr explicit operator bool(this C const& self) noexcept {
         return self.GetCount() != CountType {};
      }
   };

} // namespace Langulus::Anyness::Component
