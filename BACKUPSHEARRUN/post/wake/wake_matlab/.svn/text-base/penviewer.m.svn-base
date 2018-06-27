%@t
% \textbf{penviewer.m}
%@h
%   Description:
%     Makes plots of two pencils for comparison.
%@q

% NEEDS TO BE CLEANED UP TO BE USEFUL AGAIN

close all, clear all, clc

fname = 'phi_i0129_j0129_n00000.pen';
% fname = 'error_i0129_j0129_n00003.pen';

fid = fopen(fname,'r');
fgetl(fid); fgetl(fid); fgetl(fid);
a = fscanf(fid, '%g %g %g %g', [4 inf]);
a = a';
fclose(fid);

fname2 = 'phi_i0129_j0129_n00003.pen';

fid = fopen(fname2,'r');
fgetl(fid); fgetl(fid); fgetl(fid);
a2 = fscanf(fid, '%g %g %g %g', [4 inf]);
a2 = a2';
fclose(fid);

% a = [a;a];
% a2 = [a2;a2];
% a(259:length(a),1) = a(259:length(a),1)+258;
% a2(259:length(a2),1) = a2(259:length(a2),1)+258;

% hold on; plot(a(:,1),a(:,4),'b.');
% hold on; plot(a2(:,1),a2(:,4),'g.');
% legend('predicted solution','computed solution');
% 
% y = a(:,3);
% 
% for i = 1:length(y)
%     p(i) = 0.5*y(i)^2+ 0.5039*y(i) + 1.002;
% end
% 
% plot(y,p,'m')

yc = a2(:,3);
ye = a2(:,2);
gt = (yc(258)-yc(1));
gb = (yc(257)-yc(2));

g = gt/gb;

c1 = (sin(yc(2))-sin(yc(257)))/(yc(2)-yc(257))
p = -(sin(2*pi*yc(129))+c1*yc(129))*(sin(2*pi*yc(129))+c1*yc(129))*(sin(2*pi*yc)+c1*yc);
% p = sin(2*pi*yc(129))*sin(2*pi*yc(129))*sin(2*pi*yc);
% p(1) = p(257);
% p(258) = p(2);
% p = sin(2*pi*yc(129)*g)*sin(2*pi*yc(129)*g)*sin(2*pi*yc*g);

y2= [yc;yc]; y2(259:length(y2))=y2(259:length(y2))+y2(258);
p = [p;p]; 
plot(p,'b.'); hold on; 
% plot(yc,a2(:,4),'m.')
plot([a2(:,4);a2(:,4)],'m.')
legend('predicted solution','computed solution');

dy = yc(2)-yc(1);

c = [257,258,259];
g = [p(257),sin(2*pi*yc(129))*sin(2*pi*yc(129))*sin(2*pi*(yc(257)+dy)),sin(2*pi*yc(129))*sin(2*pi*yc(129))*sin(2*pi*(yc(257)+2*dy))];
% 
% figure,
% plot(c,p(257:259),'b.'); hold on; plot(c,[a2(257:258,4)',a2(1,4)],'m.')
% plot(c,g,'k.')
% 
% figure
% plot([p(2:257),p(2:257)],'g.'), hold on, plot([a2(2:257,4);a2(2:257,4)],'m.')
