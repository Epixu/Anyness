#pragma once
#include "Same.hpp"


namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::Bases<T>:                       
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Bases = <single type or Types<...>>;` in T 
   template<class T>
   struct Bases {
      using Type = void;
      static constexpr bool Enabled = false;
   };
}

namespace Langulus::CT
{
   namespace Inner
   {
      /// Helper function to extract reflected base types                     
      template<class T>
      consteval CT::Typelist auto GetBaseTypes() {
         static_assert(not ::std::is_reference_v<T>,
            "Strip references first");
         static_assert(not CT::Convoluted<T>,
            "Strip constness/volatility first");

         if constexpr (CTTI::Bases<T>::Enabled) {
            // Checked externally, T doesn't have to be complete        
            using LIST = typename CTTI::Bases<T>::Type;
            if constexpr (CT::Typelist<LIST>)
               return LIST {};
            else
               return Types<LIST> {};
         }
         else if constexpr (requires { typename T::CTTI_Bases; }) {
            // Checked internally, T has to be a complete type          
            using LIST = typename T::CTTI_Bases;
            if constexpr (CT::Typelist<LIST>)
               return LIST {};
            else
               return Types<LIST> {};
         }
         else return Types<void> {};
      };

      /// Check if T has BASE                                                 
      ///   @attention involves only C++ bases, not reflected ones            
      template<class T, class BASE>
      consteval bool DerivedFrom() {
         if constexpr (::std::same_as<T, BASE>)
            // Neither T nor BASE have to be complete                   
            return true;
         else
            // T has to be complete                                     
            return ::std::derived_from<Decay<T>, Decay<BASE>>;
      }
   }

   /// Check if the origin T publicly inherits (or is) all the BASE(s)        
   /// Compensates for std::derived_from not returning true for the same      
   /// primitive types...                                                     
   ///   @attention involves only C++ bases, not reflected ones               
   template<class T, class...BASE>
   concept DerivedFrom = Inner::CheckSize<BASE...>()
       and (Inner::DerivedFrom<T, BASE>() and ...);
   
   /// Check if T1 is somehow related to all of the provided types            
   ///   @attention involves only C++ bases, not reflected ones               
   template<class T1, class...TN>
   concept Related = Inner::CheckSize<TN...>() 
       and ((DerivedFrom<T1, TN> or DerivedFrom<TN, T1>) and ...);

   /// Check if a type is virtually derived from all the provided BASE(s)     
   ///   @attention involves only C++ bases, not reflected ones               
   template<class T, class...BASE>
   concept VirtuallyDerivedFrom = Inner::CheckSize<BASE...>()
       and ((::std::is_base_of_v<Decay<BASE>, Decay<T>>
         and not requires (Decay<BASE>* from) { static_cast<Decay<T>*>(from); }
       ) and ...);
   
   /// Binary compatibility check between T1 and the provided TN              
   /// To be binary compatible, types must be of the same size, and be        
   /// similar or related                                                     
   template<class T1, class...TN>
   concept BinaryCompatible = Inner::CheckSize<TN...>() and ((
         Similar<T1, TN> or (Related<T1, TN> and sizeof(T1) == sizeof(TN))
      ) and ...);
}

namespace Langulus
{
   /// Get the reflected base types, CT::Void if none                         
   template<class T>
   using BasesOf = decltype(CT::Inner::GetBaseTypes<Decvq<Deref<T>>>());
}
