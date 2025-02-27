#pragma once
#include "../Typenav.hpp"


namespace Langulus::CT
{
   namespace Inner
   {

      template<class T1, class T2>
      consteval bool NestedSimilar() {
         using Stripped1 = Decvq<Deref<T1>>;
         using Stripped2 = Decvq<Deref<T2>>;
         if constexpr (::std::same_as<Stripped1, Stripped2>)
            return true;
         else if constexpr (::std::is_pointer_v<Stripped1>
                        and ::std::is_pointer_v<Stripped2>)
            return NestedSimilar<::std::remove_pointer_t<Stripped1>,
                                 ::std::remove_pointer_t<Stripped2>>();
         else return false;
      }

   } // namespace Langulus::CT::Inner

   /// Check if all T are complete (defined), by exploiting sizeof            
   /// Usefulness of this is limited to the first instantiation, and          
   /// that is how it is used upon reflection by RTTI. Any other use is       
   /// undefined and might produce wrong results on some compilers.           
   /// Thankfully, most modern compilers do detect, if a definition           
   /// changes between completeness checks, so it is unlikely to cause any    
   /// real harm: https://stackoverflow.com/questions/21119281                
   template<class...T>
   concept Complete = ((sizeof(T) == sizeof(T)) and ...);

   /// True if decayed T1 matches all decayed TN types                        
   ///   @attention ignores type density and cv-qualifications                
   template<class T1, class...TN>
   concept Same = (::std::same_as<Decay<T1>, Decay<TN>> and ...);

   /// True if unqualified T1 matches all unqualified TN types                
   ///   @attention ignores cv-qualifications only                            
   template<class T1, class...TN>
   concept Similar = (Inner::NestedSimilar<T1, TN>() and ...);

   /// True if T1 matches exactly all the provided TN, including              
   /// density and cv-qualifiers                                              
   template<class T1, class...TN>
   concept Exact = (::std::same_as<T1, TN> and ...);

   /// True if decayed T1 matches at least one of the decayed TN              
   ///   @attention ignores type density and cv-qualifications                
   template<class T1, class...TN>
   concept SameAsOneOf = (::std::same_as<Decay<T1>, Decay<TN>> or ...);

   /// True if unqualified T1 matches at least one of the unqualified TN      
   ///   @attention ignores cv-qualifications only                            
   template<class T1, class...TN>
   concept SimilarAsOneOf = (Inner::NestedSimilar<T1, TN>() or ...);

   /// True if T1 matches exactly at least one of the TN, including           
   /// density and cv-qualifications                                          
   template<class T1, class...TN>
   concept ExactAsOneOf = (::std::same_as<T1, TN> or ...);

} // namespace Langulus::CT
