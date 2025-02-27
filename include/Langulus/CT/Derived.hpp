#pragma once
#include "Same.hpp"


namespace Langulus::CT
{
   namespace Inner
   {
      /// Check if T has BASE                                                 
      template<class T, class BASE>
      consteval bool DerivedFrom() {
         if constexpr (Same<T, BASE>)
            // Neither T nor BASE have to be complete                   
            return true;
         else
            // T has to be complete                                     
            return ::std::derived_from<Decay<T>, Decay<BASE>>;
      }
   } // namespace Langulus::CT::Inner

   /// Check if the origin T publicly inherits (or is) all the BASE(s)        
   /// Compensates for std::derived_from not returning true for the same      
   /// primitive types...                                                     
   template<class T, class...BASE>
   concept DerivedFrom = (Inner::DerivedFrom<T, BASE>() and ...);
   
   /// Check if T1 is somehow related to all of the provided types            
   template<class T1, class...TN>
   concept Related = ((DerivedFrom<T1, TN> or DerivedFrom<TN, T1>) and ...);

   /// Check if a type is virtually derived from all the provided BASE(s)     
   template<class T, class...BASE>
   concept VirtuallyDerivedFrom = ((::std::is_base_of_v<Decay<BASE>, Decay<T>>
         and not requires (Decay<BASE>* from) { static_cast<Decay<T>*>(from); }
      ) and ...);
   
   /// Binary compatibility check between T1 and the provided TN              
   /// To be binary compatible, types must be of the same size, and be        
   /// similar or related                                                     
   template<class T1, class...TN>
   concept BinaryCompatible = ((
         Similar<T1, TN> or (Related<T1, TN> and sizeof(T1) == sizeof(TN))
      ) and ...);

} // namespace Langulus::CT
